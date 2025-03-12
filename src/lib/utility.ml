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

let fold2 ~f ~init arr1 arr2 =
  let rec loop acc i =
    match i with
    | 0 -> f acc arr1.(i) arr2.(i)
    | _ -> loop (f acc arr1.(i) arr2.(i)) (i - 1)
  in
  if Array.length arr1 <> Array.length arr2 then
    invalid_arg "Array don't have the same size"
  else loop init (Array.length arr1 - 1)

let map3 f x y z =
  let lx = Array.length x in
  let ly = Array.length y in
  let lz = Array.length z in
  if lx <> ly || lx <> lz then assert false
  else
    Array.init lx (fun i -> f (Array.get x i) (Array.get y i) (Array.get z i))

let min_list f l =
  let foldf acc y =
    match acc with
    | None -> Some (y, f y)
    | Some (x, vx) ->
        let vy = f y in
        if vx <= vy then Some (x, vx) else Some (y, vy)
  in
  List.fold_left foldf None l

  let bv_to_smtlib bv = 
    let s = Bitvector.to_hexstring bv in
    String.mapi (fun i c -> if i = 0 then '#' else c) s

  let cst_to_smtlib cst size = 
    let bv = Bitvector.of_int ~size cst in
    bv_to_smtlib bv

