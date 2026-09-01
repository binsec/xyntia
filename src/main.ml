(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2019-2026                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You can redistribute it and/or modify it under the terms of the GNU   *)
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
open Xyntia

let version = "0.2.2"

let seed = ref None

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
    mutable ndjson : bool   ;
    mutable repeat: int     ;
    mutable seed: int option;
    mutable formula : Sampler.groundtruth;
    mutable smt : string option;
    mutable smt_to : int option;
    mutable check : bool;
    mutable cegis : bool;
    mutable binsec_config: string option;
    mutable binaryfile : string option;
    mutable isa : string option;
    mutable sample_out : string option;
    mutable sample_only : bool;
    mutable sygus : bool;
    mutable infrules : string;
    mutable noguidance : bool;
    mutable rules_combinator : string;
    mutable min_orig_size : int;
    mutable sse_depth : int option;
    mutable sse_max_paths : int option;
    mutable quiet : bool;
}

(** Output of Xyntia **)
let print_expr (output : Tree.variable) (smry : synthesis_result) =

    let should_write () =
        let write_to_file_in_append_mode index =
            let value = (if smry.synthesis.success then "yes" else "no") in
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

    Printf.printf "expression: %s\n" (Tree.string_of smry.synthesis.expression);
    Printf.printf "simplified: %s\n" (Tree.string_of smry.synthesis.simplified);
    Printf.printf "smtlib: %s\n" smry.synthesis.smtlib;
    Printf.printf "output: %s<%d>\n" output.name output.sz;
    Printf.printf "synthesized expression size: %d\n" smry.synthesis.size;
    Printf.printf "original expression size: %d\n" smry.target.orig_size;
    Printf.printf "success: %s\n" (if smry.synthesis.success then "yes" else "no");
    Printf.printf "synthesis time: %f\n" smry.synthesis.time_synthesis;
    Printf.printf "simplification time: %f\n" smry.synthesis.time_simplify;
    match smry.cegis with
    | Some cegis -> 
        Printf.printf "cegis equivalence checks time: %f\n" cegis.time_equiv_check;
        Printf.printf "number cegis loop: %d\n" cegis.nb_equiv_checks
    | None -> ();
    Printf.printf "equiv: %s\n" (Checker.answer_to_string smry.equiv);
    Printf.printf "number of mutations: %d\n" smry.synthesis.nb_mutations;
    Printf.printf "seed: %d\n" (Option.get !seed);
    flush stdout

(** JSON Output of Xyntia **)
let print_json (output : Tree.variable) (smry : synthesis_result) ndjson taskid nbtasks =
    let out = Printf.sprintf "%s<%d>" output.name output.sz in
    let open Yojson in
    let res = 
        match smry.cegis with 
        | Some cegis_smry -> `Assoc [ 
            ("expression", `String (Tree.string_of smry.synthesis.expression));
            ("simplified", `String (Tree.string_of smry.synthesis.simplified));
            ("smtlib", `String smry.synthesis.smtlib);
            ("output", `String out);
            ("synth_size", `Int smry.synthesis.size);
            ("orig_size", `Int smry.target.orig_size);
            ("success", `String (if smry.synthesis.success then "yes" else "no"));
            ("synthesis_time", `Float smry.synthesis.time_synthesis);
            ("simplification_time", `Float smry.synthesis.time_simplify);
            ("cegis_equiv_check_time", `Float cegis_smry.time_equiv_check);
            ("n_cegis_loop", `Int cegis_smry.nb_equiv_checks);
            ("equiv", `String (Checker.answer_to_string smry.equiv));
            ("nb_mutations", `Int smry.synthesis.nb_mutations);
            ("seed", `Int (Option.get !seed));]
        | None -> `Assoc [ 
            ("expression", `String (Tree.string_of smry.synthesis.expression));
            ("simplified", `String (Tree.string_of smry.synthesis.simplified));
            ("smtlib", `String smry.synthesis.smtlib);
            ("output", `String out);
            ("synth_size", `Int smry.synthesis.size);
            ("orig_size", `Int smry.target.orig_size);
            ("success", `String (if smry.synthesis.success then "yes" else "no"));
            ("synthesis_time", `Float smry.synthesis.time_synthesis);
            ("simplification_time", `Float smry.synthesis.time_simplify);
            ("equiv", `String (Checker.answer_to_string smry.equiv));
            ("nb_mutations", `Int smry.synthesis.nb_mutations);
            ("seed", `Int (Option.get !seed));]
    in

    let res, printer = 
        if ndjson then 
            `Assoc [ 
                ("id", `Int taskid); 
                ("total", `Int nbtasks);
                ("results", res) 
            ], (fun t -> Basic.to_string t)
        else 
            res, (fun t -> Basic.pretty_to_string t)
    in
    Printf.printf "%s\n" (printer res);
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
        ndjson = false      ;
        repeat = 1;
        seed = None;
        formula = Nothing;
        smt = None;
        smt_to = None;
        check = false;
        cegis = false;
        binsec_config = None;
        binaryfile = None;
        isa = None;
        sample_out = None;
        sample_only = false;
        sygus = false;
        infrules = "";
        noguidance = false;
        rules_combinator = "product";
        min_orig_size = -1;
        sse_depth = None;
        sse_max_paths = None;
        quiet = false;
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
            "\t{mba|expr|full|5|mba_shift|mba_ite|sygus|expr_x86|full_x86} set of operators in the grammar (default: expr)
            \tA list of operator among not, neg, +, -, *, &, |, ^, <<, >>, >>s, ror, /, /s, %, %s, ite, bswap  can also be given (e.g., -ops '+,-,*')"
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
            "-ndjson",
            Unit (fun () -> args.ndjson <- true),
            "\tNDJSON format output"
        );
        (
            "-repeat",
            Int (fun t -> args.repeat <- t),
            "\tRepeat n times until success"
        );
        (
            "-formula",
            String (fun h -> args.formula <- SmtFile h),
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
            "-sygus",
            Unit (fun () -> args.sygus <- true),
            "\treturn the sygus problem (to call other synthesizers)"
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
        (
            "-infrules",
            String (fun h -> args.infrules <- h),
            "\tList of rules among: +, +_l, *, *_l, mask, maskotf, ^, ^_l, >>u, >>u_l, <<, <<_l, ror, affine, poly2, mba (= +, maskotf, <<, *, ^), all (= +, maskotf, <<, >>u, *, ^, ror, poly2, affine)"
        );
        (
            "-infrules-final-only",
            Unit (fun () -> args.noguidance <- true),
            "\tUse the inference rules only as termination criteria (not in the guidance)"
        );
        (
            "-infrules-combinator",
            String (fun h -> args.rules_combinator <- h),
            "\tSelect the operation to combine the inference rule distances: product, min"
        );
        (
            "-min-orig",
            Int (fun t -> args.min_orig_size <- t),
            "\tSynthesize only expression bigger than specified size"
        );
        (
            "-sse-depth",
            Int (fun t -> args.sse_depth <- Some t),
            "\tSet the symbolic exec depth (used for the sampling with Binsec)"
        );
        (
            "-sse-maxpaths",
            Int (fun t -> args.sse_max_paths <- Some t),
            "\tSet the maximum of path that can be enumerated by the symbolic engine (used for the sampling with Binsec)"
        );
        (
            "-quiet",
            Unit (fun () -> args.quiet <- true),
            "\tRemove logging outputs"
        );
        (
            "-version",
            Unit (fun () -> 
                Printf.printf "%s\n" version;
                exit 0    
            ),
            "\tPrint version number"
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


let infrules_from_str s =  
    let l = String.split_on_char ',' s in
    (* filter empty string *)
    let inf_arr = List.filter_map (fun str -> let s = String.trim str in if String.equal s String.empty then None else Some s) l in
    List.fold_right (fun s acc -> 
        match s with 
        | "all" -> List.append all_inference_rules acc
        | "mba" -> List.append mba_inference_rules acc
        | s -> (Distance.str_to_infrule s)::acc
    ) inf_arr []

let to_synthesis_args (args : args_t) : synthesis_args =
    {
        heuristic = Heuristic.kind_of_string args.heuristic ;
        time = args.time;
        dist = Distance.kind_of_str args.dist;
        maxdepth = args.maxdepth;
        repeat = args.repeat;
        formula = args.formula;
        smt = args.smt;
        smt_to = args.smt_to;
        cegis = args.cegis;
        infrules = infrules_from_str args.infrules;
        noguidance = args.noguidance;
        rules_combinator = args.rules_combinator;
        check = args.check;
    }

let get_mutations args (module O : Oracle.ORACLE) = match args.ops with
| "mba" | "expr" | "full" | "5" | "mba_shift" | "mba_ite" | "sygus" | "expr_ite" | "expr_x86" | "full_x86" -> Mutations.mutations_of_str (module O: Oracle.ORACLE) args.ops
| _ -> 
    let l = String.split_on_char ',' args.ops in
    let arr = Array.init (List.length l) (fun i -> String.trim (List.nth l i)) in
    Mutations.mutations_of_operators (module O: Oracle.ORACLE) arr

let () =
    let _ = if (Array.length Sys.argv) = 1 then 
        begin
            Printf.printf "No argument given. Documentation is available through %s -help\n" Sys.argv.(0); 
            exit 1
        end in
    
    let args = parse_args () in
    Thread_logger.quiet := args.quiet;
    (match args.seed with
    | None -> (
        Random.self_init ();
        seed := Some (Random.int ((Int.shift_left 1 30)-1));
        Random.init (Option.get !seed))
    | Some s -> (
        seed := Some s;
        Random.init s));

    let samples, formula = match args.binaryfile, args.binsec_config with
    | Some b, Some c -> 
        Sampler.sample b c args.isa args.sample_out ?sse_depth:args.sse_depth ?sse_maxpaths:args.sse_max_paths ~quiet:args.quiet;
        (Sampler.get_samples ()), (Sampler.get_formula ())
    | None, None -> (
        (match args.formula, (args.check || args.cegis) with
        | Nothing, true -> failwith "When using samples directly, if you use -check you must also set -formula"
        | _ -> ());
        [Sampler.File args.samplingf], args.formula)
    | _, _ -> failwith "-bin and -config must be set together"
    in
    args.formula <- formula;

    if args.sygus then (
        List.iter (fun (sample: Sampler.samplekind) -> 
            let oracle = match sample with
            | File f -> oracle_from_file args.min_const args.max_const f 
            | Json j -> oracle_from_json args.min_const args.max_const j in
            let module O = (val oracle : Oracle.ORACLE) in
            let (module MU : Mutations.MUTATIONS) = get_mutations args (module O: Oracle.ORACLE) in
            let s = get_sygus (module O) (module MU) in
            match args.sample_out with
            | Some odir  -> 
                let outname = (O.out_var ()).name in
                let outfilename = odir ^ "/" ^ outname ^ ".sl" in
                let oc = open_out outfilename in
                Printf.fprintf oc "%s\n" s;
                close_out oc
            | None -> 
                Printf.printf "%s\n" s;
                Format.print_flush ()
        ) samples;
        exit 0
    );
    
    if args.sample_only then
        exit 0;

    List.iteri (fun id (sample:Sampler.samplekind) -> 
        Thread_logger.create "        Setup Oracle ";

        let oracle = match sample with
            | File f -> oracle_from_file args.min_const args.max_const f 
            | Json j -> oracle_from_json args.min_const args.max_const j in
        let module O = (val oracle : Oracle.ORACLE) in
        let orig_size = O.get_expr_size () in
        if Option.is_some orig_size && Option.get orig_size < args.min_orig_size then () else
        let (module MU : Mutations.MUTATIONS) = get_mutations args (module O: Oracle.ORACLE) in
        
        Thread_logger.stop ();

        let s = Printf.sprintf " Synthesizing %s " ((O.out_var ()).name) in
        Thread_logger.create s;
        
        let smry = run (module O) (module MU) (to_synthesis_args args) in
        
        Thread_logger.stop ();
        if args.json || args.ndjson then 
            print_json (O.out_var ()) smry args.ndjson id (List.length samples)
        else
            print_expr (O.out_var ()) smry
    ) samples
