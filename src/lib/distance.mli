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

type kind = LOGARITH | ARITH | XOR | HAMMING | SYNTIA

type infrule =  MinOffset | Offset | MinMul | Mul | Mask | MaskOTF | MinXor | Xor | UDivX86 | UModX86 
                | MinUShiftRight | UShiftRight | MinShiftLeft | ShiftLeft | Rotate | Affine | Poly2

module type DIST = sig
  val name : string
  val dist : Bitvector.t -> Bitvector.t -> float
  val is_zero : float -> bool
end

module type VECDIST = sig
  type t
  val name : string
  val vecdist : t Tree.t -> Bitvector.t array -> Bitvector.t array -> float
  val is_zero : float -> bool
  val extract : t Tree.t -> Bitvector.t array -> Bitvector.t array -> t Tree.t
end

module Arith : DIST
(** Arithmetic distance **)

module Hamming : DIST
(** Hamming distance **)

module Xor : DIST
(** Xor distance **)

module Logarith : DIST
(** Logarithmetic distance **)

module Syntia : DIST
(** Distance used by Syntia in the paper "Syntia: Synthesizing the Semantics of Obfuscated Code" by Blazytko et al. **)

val dist_vec : float array -> float
(** How do we aggregate the distance along a vector of input output *)

val vectorizeDist : (float array -> float) -> (module DIST) -> (module Tree.TreeConstructor with type data = 'a) -> (module VECDIST with type t = 'a)
(** Transform a bitvector distance into a (in,out) vector distance *)

module GuideModCst (D : DIST) (O : Oracle.ORACLE) (M : Tree.TreeConstructor) (MU : Mutations.MUTATIONS): sig
  module NoGuide : VECDIST with type t = M.data
  (** Distance without guiding *)

  module Offset : VECDIST with type t = M.data
  (** Guiding distance for add/sub *)

  module MinOffset : VECDIST with type t = M.data
  (** Guiding distance for add/sub searching the minimum offset instead of the first one *)

  module Xor : VECDIST with type t = M.data
  (** Guiding distance for xor *)

  module MinXor : VECDIST with type t = M.data
  (** Guiding distance for xor searching minimal distance on all the output,expected pair *)

  module Mask : VECDIST with type t = M.data
  (** Guiding distance for combined 'and' and 'or'.
      The generated masks are the one that adds the most constraints to the output.
  *)

  module MaskOTF : VECDIST with type t = M.data
  (** Guiding distance for combined 'and' and 'or' which computes the mask on-the-fly.
      This produces masks that adds the least constraints to the output.
  *)

  module ShiftLeft : VECDIST with type t = M.data
  (** Guiding distance for logical shift left *)

  module MinShiftLeft : VECDIST with type t = M.data
  (** Guiding distance for logical shift left searching minimal distance on all the possible shifts *)

  module UShiftRight : VECDIST with type t = M.data
  (** Guiding distance for logical shift right *)

  module MinUShiftRight : VECDIST with type t = M.data
  (** Guiding distance for logical shift right searching minimal distance on all the possible shifts *)

  module Mul : VECDIST with type t = M.data
  (** Guiding distance for multiplication *)

  module MinMul : VECDIST with type t = M.data
  (** Guiding distance for multiplication searching the minimum scalar instead of the first one *)

  module UDivX86 : VECDIST with type t = M.data
  (** Guiding distance for unsigned x86 division *)

  module UModX86 : VECDIST with type t = M.data
  (** Guiding distance for unsigned x86 modulo *)

  module Rotate : VECDIST with type t = M.data
  (** Guiding distance for rotate *)

  module Affine : VECDIST with type t = M.data
  (** Guiding distance for affine function *)

  module LinearComb () : VECDIST with type t = M.data
  (** Guiding distance for linear combination of variables and expression *)

  module Byteswap : VECDIST with type t = M.data
  (** Guiding distance for byteswap *)

  module Poly2 : VECDIST with type t = M.data
  (** Guiding distance for 2nd degree polynomial *)

end

(** Combines multiple vectorial distance into a single one *)
module CombineVecDist (M : sig
  type t
  val combinator : float -> float -> float
  val dists : (module VECDIST with type t = t) list
end) : VECDIST with type t = M.t

val str_to_infrule : string -> infrule
val create_infrules : infrule list -> (module DIST) -> (module Oracle.ORACLE) -> 
  (module Tree.TreeConstructor with type data = 'a) -> (module Mutations.MUTATIONS) -> (module VECDIST with type t = 'a) list

val kind_of_str : string -> kind