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

type summary = {
    success : bool ;
    expression : string ;
    simplified : string ;
    smtlib : string ;
    size : int;
    time_synthesis : float;
    time_simplify : float;
}

module type S = sig
    val search : int -> summary
end

module Mk_iterated_local_search (D: Distance.VECDIST) (O : Oracle.ORACLE) (M : Tree.MUTATOR) : S (* ils *)
module Mk_random_walk           (D : Distance.VECDIST) (O : Oracle.ORACLE) (M : Tree.MUTATOR) : S (* rw  *)
module Mk_hill_climbing         (D : Distance.VECDIST) (O : Oracle.ORACLE) (M : Tree.MUTATOR) : S (* hc  *)
module Mk_simulated_annealing   (D : Distance.VECDIST) (O : Oracle.ORACLE) (M : Tree.MUTATOR) : S (* sa  *)

val of_string : (module Distance.VECDIST) -> (module Oracle.ORACLE) -> (module Tree.MUTATOR) -> string -> (module S)
