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
    size : int ;
    time_synthesis : float ;
    time_simplify : float ;
    nb_mutations : int ;
}

module type S = sig
    val search : int -> summary
end

(* Generates a search heuristics.
   The first VECDIST is the guiding distance while the second is the final one *)
val of_string : (module Mutations.MUTATOR) -> (module Distance.VECDIST with type t = Mutations.mutd) -> (module Distance.VECDIST with type t = Mutations.mutd) -> (module Oracle.ORACLE) -> string -> (module S)
