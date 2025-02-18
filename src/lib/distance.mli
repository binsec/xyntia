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

module type DIST = sig
  val dist : Bitvector.t -> Bitvector.t -> float
  val is_zero : float -> bool
end

module type VECDIST = sig
  val vecdist : Bitvector.t array -> Bitvector.t array -> float
  val is_zero : float -> bool
  val extract : Tree.t -> Bitvector.t array -> Bitvector.t array -> Tree.t
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

val vectorizeDist : (float array -> float) -> (module DIST) -> (module VECDIST)
(** Transform a bitvector distance into a (in,out) vector distance *)
