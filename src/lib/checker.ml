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

type equiv_answer = YES | NO | UKN

let answer_to_string = function
| YES -> "yes"
| NO -> "no"
| UKN -> "ukn"

type counter_example = { 
  inputs : (Oracle.variable * Bitvector.t) array;
  output : (Oracle.variable * Bitvector.t)  
}

module type CheckerType = sig
  val open_session : unit -> unit
  val close_session : unit -> unit
  val check : string -> string -> equiv_answer
  val get_counter_example : unit -> counter_example
end

module EquivChecker (O : Oracle.ORACLE) (Solver : Smt.Smt_sig.Solver)= struct
  let solver = ref None
  let open_session () =
      match !solver with
      Some _ -> failwith "Cannot open a session twice"
      | None -> solver := Some (Solver.open_session ())
  let close_session () = 
    match !solver with
      | Some s -> Solver.close_session s; solver := None
      | None -> ()
      
  let check formula_file smtexpr = 
      let solv = match !solver with
      | Some s -> s
      | None -> failwith "Session not open"
      in

      let ovar = O.out_var () in
      let smtexpr = Format.sprintf "(define-fun cegis_checker () (_ BitVec %d) %s)" ovar.sz smtexpr in

      let orig_scr = Binsec.(Parse_utils.read_file ~parser:Smtlib_parser.script ~lexer:Smtlib_lexer.token ~filename:formula_file) in
      let smtexpr_scr = Binsec.(Parse_utils.read_string ~parser:Smtlib_parser.script ~lexer:Smtlib_lexer.token ~string:smtexpr) in
    
      let fullscr : Binsec.Smtlib.script = { 
          script_commands=List.append orig_scr.script_commands smtexpr_scr.script_commands; 
          script_loc=Binsec.Location.dummy_loc
      } in

     
      let out_term = Binsec.(Formula.(mk_bv_var (bv_var ovar.name ovar.sz))) in
      let cegis_term = Binsec.(Formula.(mk_bv_var (bv_var "cegis_checker" ovar.sz))) in

      let fm = Binsec.Smtlib_to_formula.script fullscr in
      let fm = Binsec.(Formula.push_front_assert (Formula.mk_bv_distinct out_term cegis_term) fm) in

      Binsec.Formula.iter_forward (Solver.put solv) fm;
      match Solver.check_sat solv with
      | SAT -> NO
      | UNSAT -> YES
      | TIMEOUT | UNKNOWN -> UKN
  
  let get_counter_example () = 
    let solv = match !solver with
      | Some s -> s
      | None -> failwith "Session not open"
    in
    let inputs = Array.map (fun (v : Oracle.variable) ->
      let term = Binsec.Formula.(mk_bv_var (bv_var v.name v.sz)) in
      let bv = Solver.get_bv_value solv term  in
      (v, bv)
    ) (O.vars ()) in
    let ovar = O.out_var () in
    let out_term = Binsec.(Formula.(mk_bv_var (bv_var ovar.name ovar.sz))) in
    let output = ovar, Solver.get_bv_value solv out_term  in
    { inputs; output}

end

let make_checker (module O : Oracle.ORACLE) name  timeout = 
  (if Option.is_some name then
    let solv : Binsec.Formula_options.solver = match Option.get name with
      | "z3" -> Z3
      | "cvc4" -> CVC4
      | "yices" -> Yices
      | "boolector" -> Boolector
      | "bitwuzla" -> Bitwuzla
      | _ -> failwith "Unknown smt solver"
    in
    Binsec.Formula_options.Solver.set solv);
    
  (if Option.is_some timeout then
    Binsec.Formula_options.Solver.Timeout.set (Option.get timeout));

  let module Solver = (val Smt.Smt_solver.get_solver ()) in
  (module EquivChecker (O) (Solver) : CheckerType)


