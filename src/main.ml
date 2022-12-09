(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2019-2022                                               *)
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

open Utils

type syn_fun = { f : (int array -> int) ; nargs : int }

type args_t = {
    mutable samplingf : string ;
    mutable binfile   : string ;
    mutable heuristic : string ;
    mutable time      : int    ;
    mutable nbio      : int    ;
    mutable nosamp    : bool   ;
    mutable fun_id    : int    ;
    mutable fun_nargs : int    ;
    mutable dist : string      ;
    mutable ops : string      ;
    mutable strat : string     ;
    mutable limits : int option;
    mutable min_const : int    ;
    mutable max_const : int    ;
    mutable maxdepth : int     ;
    mutable json : bool     ;
}

let set_timeout time_lim =
    if time_lim <= 0 then invalid_arg "Time limit must be positive";
    let open Sys in
    set_signal sigalrm (Signal_handle (fun _ -> raise Heuristic.Halt));
    ignore (Unix.alarm time_lim)

(** Output of Xyntia **)
let print_expr (output : Oracle.variable) (smry : Heuristic.summary) =
    Printf.printf "expression: %s\n" smry.expression;
    Printf.printf "simplified: %s\n" smry.simplified;
    Printf.printf "smtlib: %s\n" smry.smtlib;
    Printf.printf "output: %s<%d>\n" output.name output.sz;
    Printf.printf "success: %s\n" (if smry.success then "yes" else "no");
    Printf.printf "synthesis time: %f\n" smry.time_synthesis;
    Printf.printf "simplification time: %f\n" smry.time_simplify

(** JSON Output of Xyntia **)
let print_json (output : Oracle.variable) (smry : Heuristic.summary) =
    let out = Printf.sprintf "%s<%d>" output.name output.sz in
    let open Yojson in
    let res = `Assoc [ 
        ("expression", `String smry.expression);
        ("simplified", `String smry.simplified);
        ("smtlib", `String smry.smtlib);
        ("output", `String out);
        ("success", `String (if smry.success then "yes" else "no"));
        ("synthesis_time", `Float smry.time_synthesis);
        ("simplification_time", `Float smry.time_simplify);
    ] in
    Printf.printf "%s\n" (Basic.pretty_to_string res)

(** Arguments parser **)
let parse_args () =
    let args = {
        samplingf = ""    ;
        binfile   = ""    ;
        heuristic = "ils" ;
        time      = 60    ;
        nbio      = 100   ;
        nosamp    = false ;
        fun_id    = 0     ;
        fun_nargs = 0     ;
        dist = "logarith"    ;
        ops = "expr"    ;
        strat = "tiny50"  ;
        limits = None     ;
        min_const = 1     ;
        max_const = 1     ;
        maxdepth =  max_int;
        json = false      ;
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
            "-nbio",
            Int (fun t -> args.nbio <- t),
            "\tnumber of I/O metric samples (default: 100)"
        );
        (
            "-dist",
            String (fun h -> args.dist <- h),
            "\t{arith|logarith|hamm|xor|syntia} distance (default: logarith)"
        );
        (
            "-ops",
            String (fun h -> args.ops <- h),
            "\t{mba|expr|full|5|mba_shift|mba_ite} set of operators in the grammar (default: expr)"
        );
        (
            "-strat",
            String (fun h -> args.strat <- h),
            "\t{tiny50|tiny1000|full} sampling strategy (default: tiny50) [Only when synthesizing Ocaml functions]"
        );
        (
            "-limits",
            Int (fun t -> args.limits <- Some t),
            "\tnb of limit cases (default: 0) [Only when synthesizing Ocaml functions]"
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
            "-nosamp",
            Rest (
                let aux () =
                    let counter = ref 0 in
                    fun s ->
                        args.nosamp <- true;
                        incr counter;
                        if !counter = 1 then
                            args.fun_id <- int_of_string s
                        else if !counter = 2 then
                            args.fun_nargs <- int_of_string s
                in
                aux ()
            ),
            "\tindex of the function to synthesize in bin/fun.ml [Only when synthesizing Ocaml functions]"
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
    > with Ocaml function (in bin/fun.ml): ./xyntia [options] -nosamp <function index> <number of inputs> 

Options:";
        args

let () =
    let _ = if (Array.length Sys.argv) = 1 then 
        begin
            Printf.printf "No argument given. Documentation is available through %s -help\n" Sys.argv.(0); 
            exit 1
        end in

    let args = parse_args () in

    let module SStrat = (val 
        ( match args.strat with
        | "tiny50" -> (Oracle.tiny50 args.limits args.fun_nargs args.nbio)
        | "tiny1000" -> (Oracle.tiny1000 args.limits args.fun_nargs args.nbio)
        | "full" -> (Oracle.full args.limits args.fun_nargs args.nbio)
        | _ -> failwith (Printf.sprintf "Unknown sampling strat: %s" args.strat)) : (Oracle.SamplingStrat)) in

    let oracle =
        if args.nosamp then
            let f = Fun.funs.(args.fun_id) in
            let nargs = args.fun_nargs in
            (*let nargs = Fun.nargs in*)
            Oracle.of_fun ~f:f ~ninputs:nargs ~sampling_sz:args.nbio ~const_bounds:(args.min_const, args.max_const) SStrat.gen_random_int
        else
            Oracle.of_json ~filename:args.samplingf
    in
    let module O = (val oracle : Oracle.ORACLE) in
    let distance = match args.dist with
    | "arith" -> Heuristic.mk_arith ()
    | "hamm" -> Heuristic.mk_hamming ()
    | "xor" -> Heuristic.mk_xor ()
    | "logarith" -> Heuristic.mk_logarith ()
    | "syntia" -> Heuristic.mk_syntia ()
    | _ -> failwith (Printf.sprintf "Unknown heurisitic: %s" args.dist) 
    in
    
    
    let module MU = (val (match args.ops with
    | "mba" | "expr" | "full" | "5" | "mba_shift" | "mba_ite" -> Tree.mutations_of_str (module O: Oracle.ORACLE) args.ops
    | _ -> failwith (Printf.sprintf "Unknown set of operators: %s" args.ops)) : Tree.MUTATIONS)
    in


    let module M =
        (val (Heuristic.of_string
            distance (module O : Oracle.ORACLE) (module MU : Tree.MUTATIONS)
                args.heuristic)
        : Heuristic.S)
    in
    set_timeout args.time;
    let smry = M.search args.maxdepth in
    if args.json then 
        print_json (O.out_var ()) smry
    else
        print_expr (O.out_var ()) smry;
