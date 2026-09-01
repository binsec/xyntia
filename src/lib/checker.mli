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

module EquivChecker (O : Oracle.ORACLE) (S : sig val solver : Binsec.Smt_options.Solver.t val timeout : float end) () :
  CheckerType

val answer_to_string : equiv_answer -> string
val make_checker : (module Oracle.ORACLE) -> string option -> int option -> (module CheckerType)