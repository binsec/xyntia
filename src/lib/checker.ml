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

type equiv_answer = YES | NO | UKN

let answer_to_string = function
| YES -> "yes"
| NO -> "no"
| UKN -> "ukn"

type counter_example = { 
  inputs : (Tree.variable * Bitvector.t) array;
  output : Tree.variable * Bitvector.t;
}

module type CheckerType = sig
  val open_session : unit -> unit
  val close_session : unit -> unit
  val check : Sampler.groundtruth -> string -> equiv_answer
  val get_counter_example : unit -> counter_example
end

module EquivChecker (O : Oracle.ORACLE) (S : sig val solver : Binsec.Smt_options.Solver.t val timeout : float end) () = struct
  
  module Solver = (val Binsec_smtlib.Formula.Solver.create (Binsec.Smt_options.backend S.solver))
  let solver = ref None

  let open_session () =
    match !solver with
    | Some _ -> failwith "Cannot open a session twice"
    | None -> solver := Some (Solver.create ~theory:"QF_ABV" ())

  let close_session () = 
    match !solver with
    | Some s ->
      Solver.destroy s;
      solver := None
    | None -> ()

  let check (gt : Sampler.groundtruth) smtexpr = 
      let solv = match !solver with
      | Some s -> s 
      | None -> failwith "Session not open"
      in

      let ovar = O.out_var () in
      let smtexpr = Format.sprintf "(define-fun cegis_checker () (_ BitVec %d) %s)" ovar.sz smtexpr in

      let fm = match gt with
      | BinsecFormula f -> f
      | SmtFile fn -> (
        let orig_scr = Binsec.(Parse_utils.read_file ~parser:Binsec_smtlib.Lang.Parser.script 
                                ~lexer:Binsec_smtlib.Lang.Lexer.token ~filename:fn) in
        Binsec.Smtlib_to_formula.script orig_scr
      )
      | Nothing -> failwith "try to check equivalence but no groundtruth is provided" in

      let smtexpr_scr = Binsec.(Parse_utils.read_string ~parser:Binsec_smtlib.Lang.Parser.script ~lexer:Binsec_smtlib.Lang.Lexer.token ~string:smtexpr) in

     
      let out_term = Binsec_smtlib.(Formula.(mk_bv_var (bv_var ovar.name ovar.sz))) in
      let cegis_term = Binsec_smtlib.(Formula.(mk_bv_var (bv_var "cegis_checker" ovar.sz))) in

      let fm = Binsec.Smtlib_to_formula.incremental (Binsec.Smtlib_to_formula.Chunk.of_formula fm) smtexpr_scr in
      let fm = Binsec_smtlib.(Formula.push_front_assert (Formula.mk_bv_distinct out_term cegis_term) (Binsec.Smtlib_to_formula.Chunk.to_formula fm)) in

      Binsec_smtlib.Formula.iter_forward (Solver.put_entry solv) fm;
      match Solver.check_sat ~timeout:S.timeout solv with
      | SAT -> NO
      | UNSAT-> YES
      | TIMEOUT | UNKNOWN -> UKN
  
  let get_counter_example () = 
    let solv = match !solver with
      | Some s -> s
      | None -> failwith "Session not open"
    in
    let inputs = Array.map (fun (v : Tree.variable) ->
      let term = Binsec_smtlib.Formula.(mk_bv_var (bv_var v.name v.sz)) in
      let bv = Solver.get_bv_value solv [term] |> List.hd in
      (* The extracted bv might be smaller than asked: need to extend it *)
      let bv = Bitvector.extend bv v.sz in
      (v, bv)
    ) (O.vars ()) in
    let ovar = O.out_var () in
    let out_term = Binsec_smtlib.(Formula.(mk_bv_var (bv_var ovar.name ovar.sz))) in
    let bv = Solver.get_bv_value solv [out_term] |> List.hd in
    (* The extracted bv might be smaller than asked: need to extend it *)
    let bv = Bitvector.extend bv ovar.sz in
    let output = ovar, bv in
    { inputs; output}

end

let make_checker (module O : Oracle.ORACLE) name timeout = 

  let solver : Binsec.Smt_options.Solver.t = match name with 
  | Some "z3" -> Z3_smtlib
  | Some "cvc4" -> CVC4_smtlib
  | Some "yices" -> Yices_smtlib
  | Some "boolector" -> Boolector_smtlib
  | Some "bitwuzla" -> Bitwuzla_smtlib 
  | None -> Auto
  | Some _ -> failwith "Unknown smt solver" 
  in
  
  let timeout = match timeout with
  | Some i -> Float.of_int i
  | None -> Float.infinity
  in

  (module EquivChecker (O) (struct let solver = solver let timeout = timeout end) () : CheckerType)


