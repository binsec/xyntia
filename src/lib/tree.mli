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

type t
type unop_t = Minus | Not | Extend of int | Sextend of int | Reduce of int | Byteswap | Ehad | Arba | Shesh | Smol
type binop_t = Add | Sub | Mul | And | Or | Xor | RShiftu | LShift | RShifts | RotateRight
                | Div_x86 | SDiv_x86 | Mod_x86 | SMod_x86 | RShiftu_x86 | LShift_x86 | RShifts_x86
type triop_t = Div | SDiv | Mod | SMod | ITE | Im
type op_t

module type MUTATIONS = sig
    val all_mut : unit -> op_t array
    val unop_mut : unit -> op_t array
    val binop_mut : unit -> op_t array
    val triop_mut : unit -> op_t array
    val unop_sygus : op_t -> string -> int -> string
    val binop_sygus : op_t -> string -> int -> string -> int -> string
    val triop_sygus : op_t -> string -> int -> string -> int -> string -> int -> string
end


module type MUTATOR = sig
    val singleton : unit -> t
    val mutate : int -> t -> int option -> t
    val mutate_mh : int -> t -> int option -> t * float
    val cut : t -> t
    val eval : t -> Bitvector.t array
    val to_string : t -> string
    val to_smtlib : t -> string
    val simplify : t -> t
    val get_expr_size : t -> int
    val get_mutations : unit -> (module MUTATIONS)
    val mk_var : Oracle.variable -> t
    val mk_const_of_bv : Bitvector.t -> t
    val mk_unop : unop_t -> t -> t
    val mk_binop : binop_t -> t -> t -> t
    val mk_triop : triop_t -> t -> t -> t -> t
end


val mutations_of_str : (module Oracle.ORACLE) -> string -> (module MUTATIONS)

val mutations_of_operators : (module Oracle.ORACLE) -> string array -> (module MUTATIONS)

module Mk_Mutator(O : Oracle.ORACLE) (M: MUTATIONS) : MUTATOR


