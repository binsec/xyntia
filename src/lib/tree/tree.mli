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

type op_t =
  | OP_Var of Oracle.variable
  | OP_Const of Oracle.constant
  | OP_Unop of Operators.unop_t
  | OP_Binop of Operators.binop_t
  | OP_Triop of Operators.triop_t

type 'a node =
  | Var of Oracle.variable
  | Const of Oracle.constant
  | SizeMod of Operators.sizemod_t * 'a t
  | Unop of Operators.unop_t * 'a t
  | Binop of Operators.binop_t * 'a t * 'a t
  | Triop of Operators.triop_t * 'a t * 'a t * 'a t

and 'a t = {
  node : 'a node;
  (* number of nodes in this subtree, size modifiers do not count *)
  sz : int;
  (* bitsize of the values calculated by this tree *)
  bitsz : int;
  data : 'a;
}

val compare : 'a t -> 'a t -> int
val tree_equal : 'a t -> 'a t -> bool
(** Checks tree equality (with respect to the commutativity of operators) *)

val to_string : ('a -> string) -> 'a t -> string

(* Constructors *)
val mk_var : 'a -> Oracle.variable -> 'a t
val mk_const : 'a -> Oracle.constant -> 'a t
val mk_const_of_bv : 'a -> Bitvector.t -> 'a t
val mk_sizemod : 'a -> Operators.sizemod_t -> 'a t -> 'a t
val mk_unop : 'a -> Operators.unop_t -> 'a t -> 'a t
val mk_binop : 'a -> Operators.binop_t -> 'a t -> 'a t -> 'a t
val mk_triop : 'a -> Operators.triop_t -> 'a t -> 'a t -> 'a t -> 'a t

module type TreeConstructor = sig
  type data
  type t' := data t

  (* Specialized Constructor *)
  val mk_var : Oracle.variable -> t'
  val mk_const : Oracle.constant -> t'
  val mk_const_of_bv : Bitvector.t -> t'
  val mk_sizemod : Operators.sizemod_t -> t' -> t'
  val mk_unop : Operators.unop_t -> t' -> t'
  val mk_binop : Operators.binop_t -> t' -> t' -> t'
  val mk_triop : Operators.triop_t -> t' -> t' -> t' -> t'

  val eq : t' -> t' -> bool
  val compare : t' -> t' -> int

  val transfer : 'a t -> t'
end

module UnitTreeConstructor : TreeConstructor with type data = unit