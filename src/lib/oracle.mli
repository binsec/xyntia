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

type inputs = (Tree.variable * Bitvector.t) array
type sample = {
    vars : inputs;
    res  : Tree.variable * Bitvector.t         ;
}


module type ORACLE = sig
    val nvars : unit -> int
    val nconsts : unit -> int
    val nsamples : unit -> int
    val var_values : Tree.variable -> Bitvector.t array
    val const_values : Tree.constant -> Bitvector.t array
    val out_values : unit -> Bitvector.t array
    val out_var : unit -> Tree.variable
    val print : unit -> unit
    val vars : unit -> Tree.variable array
    val consts : unit -> Tree.constant array
    val get_sample : int -> sample
    val get_samples : unit -> sample array
    val get_affineplus_sample : int -> sample
    val get_affineplus_samples : unit -> sample array
    val get_affinexor_samples : unit -> sample array
    val naffineplussamples : unit -> int
    val get_affinexor_sample : int -> sample
    val naffinexorsamples : unit -> int
    val sample_output : sample -> Tree.variable * Bitvector.t
    (* TODO : Temporary, we should prevent the modification but easier for now *)
    val sample_inputs : sample -> (Tree.variable * Bitvector.t) array
    val add_sample : (Tree.variable * Bitvector.t) array -> (Tree.variable * Bitvector.t) -> unit 
    val get_expr_size : unit -> int option
end

val print_samples : sample array -> string 
val of_json : Yojson.Basic.t -> int array -> (module ORACLE)
val of_json_file : filename:string -> int array -> (module ORACLE)
val of_samples : sample array -> ?affineplus_samples:sample array -> ?affinexor_samples:sample array -> int array -> (module ORACLE)
val of_expr : unit Tree.t -> inputs array -> ?affineplus_inputs:inputs array -> ?affinexor_inputs:inputs array-> Tree.variable -> int array -> (module ORACLE)
val dummyOracle : (module ORACLE)
