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

open Operators


module Make (C : sig
  type t

  val mk_unop : unop_t -> t -> t
  val mk_binop : binop_t -> t -> t -> t
end) = struct
  open C
  let (~.) = mk_unop Not
  let (~-) = mk_unop Minus

  let (+) = mk_binop Add 
  let (-) = mk_binop Sub 
  let ( * ) = mk_binop Mul 
  let ( && ) = mk_binop And 
  let ( || ) = mk_binop Or 
  let (^) = mk_binop Xor
  let (>>) = mk_binop RShiftu
  let (<<) = mk_binop LShift
  let (>>-) = mk_binop RShifts
  let rot = mk_binop RotateRight
  let (/) = mk_binop Div_x86
  let (/-) = mk_binop SDiv_x86
  let (%) = mk_binop Mod_x86
  let (%-) = mk_binop SMod_x86
  let (>>%) = mk_binop RShiftu_x86
  let (<<%) = mk_binop LShift_x86
  let (>>-%) = mk_binop RShifts_x86

  let umax = mk_binop UMax
  let umin = mk_binop UMin
  let smax = mk_binop SMax
  let smin = mk_binop SMin
end

module UnitTreeShortCut = struct
  include Make (struct
    type t = unit Tree.t
    include Tree.UnitTreeConstructor
  end)
end 