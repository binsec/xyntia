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

module MkShortcut (C : sig
    type t

    val mk_sizemod : Operators.sizemod_t -> t -> t
    val mk_unop : Operators.unop_t -> t -> t
    val mk_binop : Operators.binop_t -> t -> t -> t
    val mk_triop : Operators.triop_t -> t -> t -> t -> t
  end) : sig
    type t := C.t
  
  (* Unop *)
  val (~.) : t -> t
  val (~-) : t -> t

  (* Binop *)
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val (^) : t -> t -> t
  val (>>) : t -> t -> t
  val (<<) : t -> t -> t
  val (>>-) : t -> t -> t
  val rot : t -> t -> t
  val (/) : t -> t -> t
  val (/-) : t -> t -> t
  val (%) : t -> t -> t
  val (%-) : t -> t -> t
  val (>>%) : t -> t -> t
  val (<<%) : t -> t -> t
  val (>>-%) : t -> t -> t
  val umax : t -> t -> t
  val umin : t -> t -> t
  val smax : t -> t -> t
  val smin : t -> t -> t
end

module TreeSimplifier (C : Tree.TreeConstructor) (O : Oracle.ORACLE) : sig
  val simplify : C.data Tree.t -> C.data Tree.t
end