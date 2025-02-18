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

module Bv = Bitvector

let is_zero x = abs_float x < 0.01

module type DIST = sig
  val dist : Bv.t -> Bv.t -> float
  val is_zero : float -> bool
end

module type VECDIST = sig
  val vecdist : Bv.t array -> Bv.t array -> float
  val is_zero : float -> bool
  val extract : Tree.t -> Bv.t array -> Bv.t array -> Tree.t
end

let bit_count x = Bitvector.fold (fun acc b -> if b then acc + 1 else acc) 0 x

(** Retrieve the number of set bit at the start of the Bv (in little-endian).
    For example : trailing_ones 0b00011 = 2 
                  trailing_ones 0b11010 = 0 
*)
let trailing_ones x =
  let rec loop acc x =
    (* The loop is not infinite as any number shifted enough time is equal to zero
       and thus has its first bit equal to zero *)
    let bit = Bv.get_bit x 0 in
    if not bit then acc else loop (acc + 1) (Bv.shift_right x 1)
  in
  loop 0 x

(** Retrieve the number of unset bit at the start of the Bv (in little-endian).
    For example : trailing_zeros 0b00011 = 0 
                  trailing_zeros 0b11010 = 1 
*)
let trailing_zeros x = trailing_ones (Bv.lognot x)

(** Retrieve the number of set bit at the end of the Bv (in little-endian).
    For example : leading_ones 0b00011 = 0 
                  leading_ones 0b11010 = 2 
*)
let leading_ones x =
  let size = Bv.size_of x - 1 in
  let rec loop acc x =
    (* The loop is not infinite as any number shifted enough time is equal to zero
       and thus has its last bit equal to zero *)
    let bit = Bv.get_bit x size in
    if not bit then acc else loop (acc + 1) (Bv.shift_left x 1)
  in
  loop 0 x

(** Retrieve the number of unset bit at the end of the Bv (in little-endian).
    For example : leading_zeros 0b00011 = 2 
                  leading_zeros 0b11010 = 0 
*)
let leading_zeros x = leading_ones (Bv.lognot x)

(** Arithmetic distance **)
module Arith : DIST = struct
  let dist x y = Bv.sub x y |> Bitvector.to_float |> Float.abs
  let is_zero = is_zero
end

(** Hamming distance **)
module Hamming : DIST = struct
  let dist x y =
    let xored = Bv.logxor x y in
    let hamming = bit_count xored in
    Float.of_int hamming

  let is_zero = is_zero
end

(** Xor distance **)
module Xor : DIST = struct
  let dist x y =
    let xored = Bitvector.logxor x y in
    Bitvector.to_float xored |> Float.abs

  let is_zero = is_zero
end

(** Logarithmetic distance **)
module Logarith : DIST = struct
  let dist x y =
    let arithme = Arith.dist x y in
    Float.log (arithme +. 1.) /. Float.log 2.

  let is_zero = is_zero
end

(** Distance used by Syntia in the paper "Syntia: Synthesizing the Semantics of Obfuscated Code" by Blazytko et al. **)
module Syntia : DIST = struct
  let dist_arith x y =
    let n = Arith.dist x y in
    let fl_sz = Bv.size_of x |> Float.of_int in
    let divisor = (2. ** fl_sz) -. 1. in
    n /. divisor

  let dist_hamm x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let hamming = Hamming.dist x y in
    hamming /. fl_sz

  let trailing_ones_dist x y =
    let size = Bv.size_of x in
    let fl_sz = size |> Float.of_int in
    let lz_x = trailing_ones x |> Float.of_int in
    let lz_y = trailing_ones y |> Float.of_int in
    Float.abs (lz_x -. lz_y) /. fl_sz

  let trailing_zeros_dist x y =
    let size = Bv.size_of x in
    let fl_sz = size |> Float.of_int in
    let lz_x = trailing_zeros x |> Float.of_int in
    let lz_y = trailing_zeros y |> Float.of_int in
    Float.abs (lz_x -. lz_y) /. fl_sz

  let leading_ones_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let fo_x = leading_ones x |> Float.of_int in
    let fo_y = leading_ones y |> Float.of_int in
    Float.abs (fo_x -. fo_y) /. fl_sz

  let leading_zeros_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let fz_x = leading_zeros x |> Float.of_int in
    let fz_y = leading_zeros y |> Float.of_int in
    Float.abs (fz_x -. fz_y) /. fl_sz

  let dist x y =
    (dist_arith x y +. dist_hamm x y +. trailing_zeros_dist x y
   +. trailing_ones_dist x y +. leading_zeros_dist x y +. leading_ones_dist x y
    )
    /. 6.

  let is_zero x = abs_float x = 0.0
end

(** How do we aggregate the distance along a vector of input output *)
let dist_vec = Array.fold_left ( +. ) 0.

let vectorizeDist (dist_agg : float array -> float) (module D : DIST) :
    (module VECDIST) =
  let module M = struct
    let vecdist actual expected = Array.map2 D.dist actual expected |> dist_agg
    let extract tree _ _ = tree
    let is_zero = D.is_zero
  end in
  (module M)
