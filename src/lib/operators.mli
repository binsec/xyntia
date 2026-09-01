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

type sizemod_t = 
| Extend of int
| Sextend of int
| Reduce of int

type unop_t =
  | Minus
  | Not
  | Byteswap
  | Ehad  (* >> 1 *)
  | Arba  (* >> 4 *)
  | Shesh (* >> 16 *)
  | Smol  (* << 1 *)

type binop_t =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | Xor
  | RShiftu
  | LShift
  | RShifts
  | RotateRight
  | Div_x86
  | SDiv_x86
  | Mod_x86
  | SMod_x86
  | RShiftu_x86
  | LShift_x86
  | RShifts_x86
  | UMin
  | SMin
  | UMax
  | SMax

type triop_t = Div | SDiv | Mod | SMod | ITE | Im

type 'a binop_side = Left of 'a | Right of 'a | Both of 'a
type 'a fromout = { from: 'a binop_side; out:'a }

module SizeMod : sig
  type t = sizemod_t
  val compare : t -> t -> int

  val to_string : t -> string
  val apply : t -> Bitvector.t -> Bitvector.t
  val to_sygus : t -> string -> int -> string

  val eq : t -> t -> bool
end

module Unop : sig
  type t = unop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t option
  val to_sygus : t -> string -> int -> string

  val eq : t -> t -> bool

  val apply : t -> Bitvector.t -> Bitvector.t
  val get_bitsz : t -> int -> int

  (* True if the operator is involutif (f f (x) = x), used for simplification 
     Default value is safe but reduces simplification *)
  val is_involutif : t -> bool

  val rand : unit -> t
  val cardinal : unit -> int
end

module Binop : sig
  type t = binop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val is_infix : t -> bool
  (** Return true if the operator is to be printed in infix position, otherwise the operator is printed in prefix position *)

  val to_string : t -> string
  val of_string : string -> t option
  val to_sygus : t -> string -> int -> string -> int -> string

  val eq : t -> t -> bool

  val apply : t -> Bitvector.t -> Bitvector.t -> Bitvector.t
  val get_bitsz : t -> int -> int -> int

  (* True if the operator is commutatif (f(x,y) = f(y,x)), used for simplification 
     Default value is safe but reduces simplifications *)
  val is_commut : t -> bool

  (* True if the operator is associatif (f(x,f(y,z)) = f(f(x,y),z)), used for simplification 
     Default value is safe but reduces simplifications *)
  val is_assoc : t -> bool

  (* True if the operator is idempotent (f(x,x) = x), used for simplification 
     Default value is safe but reduces simplifications *)
  val is_idempotent : t -> bool

  (* True if the operator is nilpotent (f(x,x) = 0), used for simplification 
     Default value is safe but reduces simplifications *)
  val is_nilpotent : t -> bool

  (* Check if the bv is a neutral element (for the provided direction) of the operator (f(x, bv) = x (right) or f(bv,x) = x (left)). 
     Used for simplification. Default value is safe but reduces simplifications *)
  val is_neutral_element : t  -> ?dir:unit binop_side -> Bitvector.t -> bool

  (* Check if the bv is an absorbing element (for the provided direction) of the operator (f(x, bv) = bv (right) or f(bv, x) = bv (left)).
     Used for simplification. Default value is safe but reduces simplifications *)
  val is_absorbing_element : t -> ?dir:unit binop_side -> Bitvector.t -> bool
  
  (* Return the sided neutral element
     E.g., if f(x, bv) = x returns Right bv
          if f(bv, x) = x returns Left bv
          if both returns Both bv
  *)
  val get_neutral : t -> int -> Bitvector.t binop_side option

   val has_neutral : t -> bool

  (* Return the annihilator 
     E.g., if f(x, bv1) = bv2 returns {from=(Right bv); out=bv2} 
  *)
  val get_annihilator : t -> int -> Bitvector.t fromout option

  val has_annihilator : t -> bool

  val inversible : t -> bool
  
  (*
      (inverse bv1 bv2) = bv3 if bv3 op bv2 = bv1
      (used in preprocess)
  *)
  val inverse : t -> Bitvector.t -> Bitvector.t -> Bitvector.t option

  val rand : unit -> t
  val cardinal : unit -> int
end

module Triop : sig
  type t = triop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t option
  val to_sygus : t -> string -> int -> string -> int -> string -> int -> string

  val eq : t -> t -> bool

  val apply : t -> Bitvector.t -> Bitvector.t -> Bitvector.t -> Bitvector.t
  val get_bitsz : t -> int -> int -> int -> int

  val rand : unit -> t
  val cardinal : unit -> int
end