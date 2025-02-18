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

type variable = private { name : string ; sz : int }
type constant = { name : string ; value : Bitvector.t }
type sample

module type ORACLE = sig
    val nvars : unit -> int
    val nconsts : unit -> int
    val nsamples : unit -> int
    val var_values : variable -> Bitvector.t array
    val const_values : constant -> Bitvector.t array
    val out_values : unit -> Bitvector.t array
    val out_var : unit -> variable
    val random_var : unit -> variable
    val random_const : unit -> constant
    val print : unit -> unit
    val vars : unit -> variable array
    val consts : unit -> constant array
    val ops : unit -> string list option
    val const_of_int : int -> int -> constant
    val const_of_bitv : Bitvector.t -> constant
    val get_sample : int -> sample
    val sample_output : sample -> variable * Bitvector.t
    (* TODO : Temporary, we should prevent the modification but easier for now *)
    val sample_inputs : sample -> (variable * Bitvector.t) array
    val add_sample : (variable * Bitvector.t) array -> (variable * Bitvector.t) -> unit 
    val get_expr_size : unit -> int option
end

val of_json : filename:string -> int array -> (module ORACLE)
