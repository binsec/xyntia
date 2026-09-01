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

type mutd = {
  mutable last_mutation_indice : int option;
  all_choices : Tree.op_t array;
  mutable left_choices : Tree.op_t array;
  vals : Bitvector.t array;
}

module type MUTATIONS = sig
  val all_mut : unit -> Tree.op_t array
  val unop_mut : unit -> Tree.op_t array
  val binop_mut : unit -> Tree.op_t array
  val triop_mut : unit -> Tree.op_t array
  val unop_sygus : Tree.op_t -> string -> int -> string
  val binop_sygus : Tree.op_t -> string -> int -> string -> int -> string
  val triop_sygus : Tree.op_t -> string -> int -> string -> int -> string -> int -> string
end

module type MUTATOR = sig
  include Tree.TreeConstructor with type data = mutd
  type t := mutd Tree.t

  val singleton : unit -> t
  val mutate : int -> t -> int option -> t
  val mutate_mh : int -> t -> int option -> t * float
  val cut : t -> t
  val eval : t -> Bitvector.t array
  val to_string : t -> string
  val to_smtlib : t -> string
  val get_expr_size : t -> int
  val get_mutations : unit -> (module MUTATIONS)
end

val mutations_of_str : (module Oracle.ORACLE) -> string -> (module MUTATIONS)

val mutations_of_operators :
  (module Oracle.ORACLE) -> string array -> (module MUTATIONS)

module Mk_Mutator (O : Oracle.ORACLE) (M : MUTATIONS) : MUTATOR