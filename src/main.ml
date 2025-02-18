(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2019-2025                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Xyntia_utils

type cegis_summary = {
    is_used : bool;
    nb_equiv_checks : int;
    time_equiv_check : float;
    equiv : Checker.equiv_answer;  
}

type args_t = {
    mutable samplingf : string ;
    mutable heuristic : string ;
    mutable time      : int    ;
    mutable dist : string      ;
    mutable ops : string      ;
    mutable min_const : int    ;
    mutable max_const : int    ;
    mutable maxdepth : int     ;
    mutable json : bool     ;
    mutable repeat: int     ;
    mutable seed: int option;
    mutable formula : string option;
    mutable smt : string option;
    mutable smt_to : int option;
    mutable check : bool;
    mutable cegis : bool;
    mutable binsec_config: string option;
    mutable binaryfile : string option;
    mutable isa : string option;
    mutable sample_out : string option;
    mutable sample_only : bool;
}

let n_cegis_loop = ref 1
let set_timeout time_lim =
    if time_lim <= 0 then invalid_arg "Time limit must be positive";
    let open Sys in
    set_signal sigalrm (Signal_handle (fun _ -> raise Exceptions.Halt));
    ignore (Unix.alarm time_lim)


let set_cegis_timeout time_lim =
    (* Only set the handler to catch abort signals 
        The signal must be send from the terminal, for example with
        timeout -s ABRT <timeout> <cmd>
    *)
    if time_lim <= 0 then invalid_arg "Time limit must be positive";
    let open Sys in
    set_signal sigabrt (Signal_handle (fun _ -> raise Exceptions.CEGISHalt))

let remove_timeout () = 
    let open Sys in
    set_signal sigalrm (Signal_handle (fun _ -> ()));
    set_signal sigabrt (Signal_handle (fun _ -> ()))

(** Output of Xyntia **)
let print_expr (output : Oracle.variable) (smry : Heuristic.summary) cegis_smry orig_expr_size =

    let should_write () =
        let write_to_file_in_append_mode index =
            let value = (if smry.success then "yes" else "no") in
            let text = index^";"^value^"\n" in
            let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 "result.data" in
            output_string oc text;
            close_out oc
        in

        match Sys.getenv "CURRENT_INDEX" with
        | exception Not_found -> ()
        | x -> write_to_file_in_append_mode x
    in

    let () = should_write () in



    Printf.printf "expression: %s\n" smry.expression;
    Printf.printf "simplified: %s\n" smry.simplified;
    Printf.printf "smtlib: %s\n" smry.smtlib;
    Printf.printf "output: %s<%d>\n" output.name output.sz;
    Printf.printf "synthesized expression size: %d\n" smry.size;
    Printf.printf "original expression size: %d\n" orig_expr_size;
    Printf.printf "success: %s\n" (if smry.success then "yes" else "no");
    Printf.printf "synthesis time: %f\n" smry.time_synthesis;
    Printf.printf "simplification time: %f\n" smry.time_simplify;
    if cegis_smry.is_used then (
        Printf.printf "cegis equivalence checks time: %f\n" cegis_smry.time_equiv_check;
        Printf.printf "number cegis loop: %d\n" cegis_smry.nb_equiv_checks
    );
    Printf.printf "equiv: %s\n" (Checker.answer_to_string cegis_smry.equiv);
    flush stdout

(** JSON Output of Xyntia **)
let print_json (output : Oracle.variable) (smry : Heuristic.summary) cegis_smry orig_expr_size =
    let out = Printf.sprintf "%s<%d>" output.name output.sz in
    let open Yojson in
    let res = if cegis_smry.is_used then `Assoc [ 
            ("expression", `String smry.expression);
            ("simplified", `String smry.simplified);
            ("smtlib", `String smry.smtlib);
            ("output", `String out);
            ("synth_size", `Int smry.size);
            ("orig_size", `Int orig_expr_size);
            ("success", `String (if smry.success then "yes" else "no"));
            ("synthesis_time", `Float smry.time_synthesis);
            ("simplification_time", `Float smry.time_simplify);
            ("cegis_equiv_check_time", `Float cegis_smry.time_equiv_check);
            ("n_cegis_loop", `Int cegis_smry.nb_equiv_checks);
            ("equiv", `String (Checker.answer_to_string cegis_smry.equiv));]
    else `Assoc [ 
            ("expression", `String smry.expression);
            ("simplified", `String smry.simplified);
            ("smtlib", `String smry.smtlib);
            ("output", `String out);
            ("synth_size", `Int smry.size);
            ("orig_size", `Int orig_expr_size);
            ("success", `String (if smry.success then "yes" else "no"));
            ("synthesis_time", `Float smry.time_synthesis);
            ("simplification_time", `Float smry.time_simplify);
            ("equiv", `String (Checker.answer_to_string cegis_smry.equiv));]
    in

    Printf.printf "%s\n" (Basic.pretty_to_string res);
    flush stdout

(** Arguments parser **)
let parse_args () =
    let args = {
        samplingf = ""    ;
        heuristic = "ils" ;
        time      = 60    ;
        dist = "logarith"    ;
        ops = "expr"    ;
        min_const = 1     ;
        max_const = 1     ;
        maxdepth =  max_int;
        json = false      ;
        repeat = 1;
        seed = None;
        formula = None;
        smt = None;
        smt_to = None;
        check = false;
        cegis = false;
        binsec_config = None;
        binaryfile = None;
        isa = None;
        sample_out = None;
        sample_only = false;
    }
    in
    let open Arg in
    parse (align [
        (
            "-heur",
            String (fun h -> args.heuristic <- h),
            "\t{ils|hc|rw|sa|mh} S-metaheuristic (default: ils)"
        );
        (
            "-time",
            Int (fun t -> args.time <- t),
            "\ttime limit in seconds (default: 60s)"
        );
        (
            "-dist",
            String (fun h -> args.dist <- h),
            "\t{arith|logarith|hamm|xor|syntia} distance (default: logarith)"
        );
        (
            "-ops",
            String (fun h -> args.ops <- h),
            "\t{mba|expr|full|5|mba_shift|mba_ite} set of operators in the grammar (default: expr)
            \tA list of operator among not, neg, +, -, *, &, |, ^, <<, >>, >>s, /, /s, %, %s, ite, bswap  can also be given (e.g., -ops '+,-,*')"
        );
        (
            "-min-const",
            Int (fun t -> args.min_const <- t),
            "\tminimum constant value (default: 1)"
        );
        (
            "-max-const",
            Int (fun t -> args.max_const <- t),
            "\tmaximum constant value (default: 1)"
        );
        (
            "-max-depth",
            Int (fun t -> args.maxdepth <- t),
            "\tmaximum depth of AST (default: max int)"
        );
        (
            "-json",
            Unit (fun () -> args.json <- true),
            "\tJSON format output"
        );
        (
            "-repeat",
            Int (fun t -> args.repeat <- t),
            "\tRepeat n times until success"
        );
        (
            "-formula",
            String (fun h -> args.formula <- Some h),
            "\tpath to a smtlib formula to check equivalence"
        );
        (
            "-smt",
            String (fun h -> args.smt <- Some h),
            "\tname of the smt solver (used for CEGIS)"
        );
        (
            "-smt-timeout",
            Int (fun h -> args.smt_to <- Some h),
            "\ttimeout used for the smt solver (used for CEGIS)"
        );
        (
            "-check",
            Unit (fun () -> args.check <- true),
            "\tCheck if the result is equivalent"
        );
        (
            "-cegis",
            Unit (fun () -> args.cegis <- true),
            "\tLaunch Xyntia in CEGIS mode"
        );
        (
            "-config",
            String (fun h -> args.binsec_config <- Some h),
            "\tConfigfile to sample the binary"
        );
        (
            "-bin",
            String (fun h -> args.binaryfile <- Some h),
            "\tbinary to sample"
        );
        (
            "-isa",
            String (fun h -> args.isa <- Some h),
            "\tisa of the binary"
        );
        (
            "-sampleout",
            String (fun h -> args.sample_out <- Some h),
            "\toutput directory to put the samples"
        );
        (
            "-seed",
            Int (fun t -> args.seed <- Some t),
            "\tProvides a seed for the random generator"
        );
        (
            "-sample-only",
            Unit (fun () -> args.sample_only <- true),
            "\tOnly sample"
        );
    ])
    (fun h -> args.samplingf <- h)
        "
$$\\   $$\\                      $$\\     $$\\           
$$ |  $$ |                     $$ |    \\__|          
\\$$\\ $$  |$$\\   $$\\ $$$$$$$\\ $$$$$$\\   $$\\  $$$$$$\\  
 \\$$$$  / $$ |  $$ |$$  __$$\\\\_$$  _|  $$ | \\____$$\\ 
 $$  $$<  $$ |  $$ |$$ |  $$ | $$ |    $$ | $$$$$$$ |
$$  g$$ \\ $$ |  $$ |$$ |  $$ | $$ |$$\\ $$ |$$  __$$ |
$$ /  $$ |\\$$$$$$$ |$$ |  $$ | \\$$$$  |$$ |\\$$$$$$$ |
\\__|  \\__| \\____$$ |\\__|  \\__|  \\____/ \\__| \\_______|
          $$\\   $$ |                                 
          \\$$$$$$  |   The Search-based Local Blackbox Deobfuscator
           \\______/                                  


Developers: Grégoire Menguy, Cauim de Souza Lima 

Usage: 
    > with a json file: ./xyntia [options] file.json 
    > with a binary: ./xyntia [options] -bin <bin> -config <ini>
Options:";
        args

let run_task args samplefile formula = 
    let csts : int array = Array.init 
        (args.max_const - args.min_const) 
        (fun v -> v + args.min_const)  
    in
    let oracle = Oracle.of_json ~filename:samplefile csts in
    let module O = (val oracle : Oracle.ORACLE) in

    let distance : (module Distance.DIST) = 
        match args.dist with
        | "arith" -> (module Distance.Arith)
        | "hamm" -> (module Distance.Hamming)
        | "xor" -> (module Distance.Xor)
        | "logarith" -> (module Distance.Logarith)
        | "syntia" -> (module Distance.Syntia)
        | _ -> failwith (Printf.sprintf "Unknown heurisitic: %s" args.dist) 
    in
    
    let (module MU : Tree.MUTATIONS) = 
        match args.ops with
        | "mba" | "expr" | "full" | "5" | "mba_shift" | "mba_ite" | "sygus" | "expr_ite" -> Tree.mutations_of_str (module O: Oracle.ORACLE) args.ops
        | _ -> 
            let l = String.split_on_char ',' args.ops in
            let arr = Array.init (List.length l) (fun i -> String.trim (List.nth l i)) in
            Tree.mutations_of_operators (module O: Oracle.ORACLE) arr
    in

    let module M = Tree.Mk_Mutator (O) (MU) in 
    
    let (module D : Distance.VECDIST) = 
        Distance.vectorizeDist Distance.dist_vec distance in

    let (module S : Heuristic.S) = Heuristic.of_string 
        (module D) (module O) (module M) args.heuristic
    in

    let (module EChecker : Checker.CheckerType) = 
        Checker.make_checker (module O) args.smt args.smt_to in

    let run_loop () =
        set_timeout args.time;
        S.search args.maxdepth

    in

    let cumul_synth_time = ref 0.0 in
    let cumul_simpl_time = ref 0.0 in
    let cumul_echeck_time = ref 0.0 in

    let rec main_loop n =
        let smry = run_loop () in
        cumul_synth_time := !cumul_synth_time +. smry.time_synthesis;
        cumul_simpl_time := !cumul_simpl_time +. smry.time_simplify;
        if n <= 1 || smry.success then
            smry
        else
            main_loop (n-1)   
    in

    let cegis_loop filename = 
        let rec aux prev : Heuristic.summary option * Checker.equiv_answer = 
            try 
                let smry = main_loop args.repeat in (* Stops (with success=no) either if the synthesis timeout or the CEGIStimeout is reached *)
                try
                    if not smry.success then Some smry, NO
                    else (
                        EChecker.open_session ();
                        let start = Sys.time () in
                        let equiv = EChecker.check filename smry.smtlib in
                        let checktime = (Sys.time ()) -. start in
                        cumul_echeck_time := !cumul_echeck_time +. checktime;
                        match equiv with 
                        | NO -> 
                            let ce = EChecker.get_counter_example () in
                            O.add_sample ce.inputs ce.output;
                            EChecker.close_session ();
                            n_cegis_loop := !n_cegis_loop+1; 
                            aux (Some smry)
                        | e -> EChecker.close_session (); Some smry, e
                    );
                with Exceptions.CEGISHalt -> 
                    EChecker.close_session ();
                    Some smry, (if smry.success then UKN else NO)
            with Exceptions.SynthesisNotStarted -> prev, NO
        in
        aux None
    in
    
    let smry_opt, isequiv = match args.cegis, formula with
    | true, Some filename -> (set_cegis_timeout args.time; cegis_loop filename)
    | true, None -> failwith "-cegis requires -formula" 
    | false, _ -> 
        try 
            let r = main_loop args.repeat in
            Some r, (if r.success then UKN else NO)
        with Exceptions.SynthesisNotStarted -> None, NO
    in
    remove_timeout ();
    
    ignore (if Option.is_none smry_opt then (print_endline "Not enough time to start synthesis"; exit 0));
    let smry = Option.get smry_opt in

    let isequiv = 
        (if args.check && Option.is_none formula then failwith "-check requires -formula");
        if isequiv = UKN && args.check && Option.is_some formula then (
            EChecker.open_session ();
            let r = EChecker.check (Option.get formula) smry.smtlib in
            EChecker.close_session ();
            r
        ) else isequiv 
    in
    
    let full_smry : Heuristic.summary = { 
        success = smry.success; 
        expression = smry.expression;
        simplified = smry.simplified;
        smtlib = smry.smtlib;
        size = smry.size;
        time_synthesis = !cumul_synth_time;
        time_simplify = !cumul_simpl_time;
    } in

    let cegis_smry : cegis_summary = {
        is_used = args.cegis;
        nb_equiv_checks = !n_cegis_loop;
        time_equiv_check = !cumul_echeck_time;
        equiv = isequiv;
    } in

    let origsize = match O.get_expr_size () with 
    | Some s -> s
    | None -> -1
    in

    if args.json then 
        print_json (O.out_var ()) full_smry cegis_smry origsize
    else
        print_expr (O.out_var ()) full_smry cegis_smry origsize


let () =

    let _ = if (Array.length Sys.argv) = 1 then 
        begin
            Printf.printf "No argument given. Documentation is available through %s -help\n" Sys.argv.(0); 
            exit 1
        end in

    let args = parse_args () in
    (match args.seed with
    | None -> Random.self_init ()
    | Some seed -> Random.init seed);


    let samples, formula = match args.binaryfile, args.binsec_config with
    | Some b, Some c -> 
        Sampler.sample b c args.isa args.sample_out;
        (Sampler.get_samples ()),Some (Sampler.get_formula ())
    | None, None -> [args.samplingf], args.formula
    | _, _ -> failwith "-bin and -config must be set together"
    in

    if args.sample_only then
        exit 0;

    List.iter (fun sample -> run_task args sample formula) samples;

    if Option.is_none args.sample_out then
        match args.binaryfile, args.binsec_config with
        | Some _, Some _ -> Sampler.remove_samples ()
        | None, None -> ()
        | _, _ -> failwith "-bin and -config must be set together"
