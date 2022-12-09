(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2019-2022                                               *)
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

exception Halt

type summary = {
    success : bool ;
    expression : string ;
    simplified : string ;
    smtlib : string ;
    time_synthesis : float;
    time_simplify : float;
}

module type Dist = sig
    val dist : Bitvector.t -> Bitvector.t -> float
    val is_zero : float -> bool
end

module type S = sig
    val search : int -> summary
end

val mk_arith    : unit -> (module Dist)
val mk_hamming  : unit -> (module Dist)
val mk_xor      : unit -> (module Dist)
val mk_logarith : unit -> (module Dist)
val mk_syntia   : unit -> (module Dist)

module Mk_iterated_local_search (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S (* ils *)
module Mk_random_walk           (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S (* rw  *)
module Mk_hill_climbing         (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S (* hc  *)
module Mk_simulated_annealing   (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S (* sa  *)

val of_string : (module Dist) -> (module Oracle.ORACLE) -> (module Tree.MUTATIONS) -> string -> (module S)
