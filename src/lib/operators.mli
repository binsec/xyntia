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

module SizeMod : sig
  type t := sizemod_t

  val compare : t -> t -> int

  val to_string : t -> string
  val apply : t -> Bitvector.t -> Bitvector.t
  val to_sygus : t -> string -> int -> string

  val eq : t -> t -> bool
end

module Unop : sig
  type t := unop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t option
  val apply : t -> Bitvector.t -> Bitvector.t
  val to_sygus : t -> string -> int -> string

  val eq : t -> t -> bool

  (* True if the operator is involutif (f f (x) = x), used for simplification 
     Default value is safe but reduces simplification *)
  val is_involutif : t -> bool
end

module Binop : sig
  type t := binop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t option
  val apply : t -> Bitvector.t -> Bitvector.t -> Bitvector.t
  val to_sygus : t -> string -> int -> string -> int -> string

  val eq : t -> t -> bool

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

  (* Check if the bv is a right neutral element of the operator (f(x, bv) = x), used for simplification 
     Default value is safe but reduces simplifications *)
  val neutral_element : t -> Bitvector.t -> bool

  (* Check if the bv is a right absorbing element of the operator (f(x, bv) = bv), used for simplification 
     Default value is safe but reduces simplifications *)
  val absorbing_element : t -> Bitvector.t -> bool
end

module Triop : sig
  type t := triop_t

  val number : int
  val of_int : int -> t
  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t option
  val apply : t -> Bitvector.t -> Bitvector.t -> Bitvector.t -> Bitvector.t
  val to_sygus : t -> string -> int -> string -> int -> string -> int -> string

  val eq : t -> t -> bool
end

module Mk_Unop (O : Oracle.ORACLE) : sig
  include module type of Unop

  val rand : unit -> unop_t
  val cardinal : unit -> int
end

module Mk_Binop (O : Oracle.ORACLE) : sig
  include module type of Binop

  val rand : unit -> binop_t
  val cardinal : unit -> int
end

module Mk_Triop (O : Oracle.ORACLE) : sig
  include module type of Triop

  val rand : unit -> triop_t
  val cardinal : unit -> int
end
