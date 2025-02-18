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

include Binsec.Bitvector

let is_even x = not (get_bit x 0)

let mod_inverse x = 
  let f8bits x =
    let onee = of_int ~size:8 1 in
    let twoo = of_int ~size:8 2 in
    let three = of_int ~size:8 3 in
    let x0 = mul three x in
    let x0 = logxor x0 twoo in
    let y = mul x x0 in
    let y = sub onee y in

    let x1 = add onee y in
    let x1 = mul x0 x1 in
    if equal (mul x1 x) onee then
      x1
    else
      assert false
  in


  let f16bits x =
    let onee = of_int ~size:16 1 in
    let twoo = of_int ~size:16 2 in
    let three = of_int ~size:16 3 in
    let x0 = mul three x in
    let x0 = logxor x0 twoo in
    let y = mul x x0 in
    let y = sub onee y in
    let x1 = add onee y in
    let x1 = mul x0 x1 in
    let y = mul y y in
    let x2 = add onee y in
    let x2 = mul x1 x2 in
    if equal (mul x2 x) onee then
      x2
    else
      assert false
  in

  let f32bits x =
    let onee = of_int ~size:32 1 in
    let twoo = of_int ~size:32 2 in
    let three = of_int ~size:32 3 in
    let x0 = mul three x in
    let x0 = logxor x0 twoo in
    let y = mul x x0 in
    let y = sub onee y in
    let x1 = add onee y in
    let x1 = mul x0 x1 in
    let y = mul y y in
    let x2 = add onee y in
    let x2 = mul x1 x2 in
    let y = mul y y in
    let x3 = add onee y in
    let x3 = mul x2 x3 in
    
    if equal (mul x3 x) onee then
      x3
    else
      assert false
  in
  let f64bits x =
    let onee = of_int ~size:64 1 in
    let twoo = of_int ~size:64 2 in
    let three = of_int ~size:64 3 in
    let x0 = mul three x in
    let x0 = logxor x0 twoo in
    let y = mul x x0 in
    let y = sub onee y in
    let x1 = add onee y in
    let x1 = mul x0 x1 in
    let y = mul y y in
    let x2 = add onee y in
    let x2 = mul x1 x2 in
    let y = mul y y in
    let x3 = add onee y in
    let x3 = mul x2 x3 in
    let y = mul y y in
    let x4 = add onee y in
    let x4 = mul x3 x4 in

    if equal (mul x4 x) onee then
      x4
    else
      assert false
  in

  let f128bits x =
    let onee = of_int ~size:128 1 in
    let twoo = of_int ~size:128 2 in
    let three = of_int ~size:128 3 in
    let x0 = mul three x in
    let x0 = logxor x0 twoo in
    let y = mul x x0 in
    let y = sub onee y in
    let x1 = add onee y in
    let x1 = mul x0 x1 in
    let y = mul y y in
    let x2 = add onee y in
    let x2 = mul x1 x2 in
    let y = mul y y in
    let x3 = add onee y in
    let x3 = mul x2 x3 in
    let y = mul y y in
    let x4 = add onee y in
    let x4 = mul x3 x4 in
    let y = mul y y in
    let x5 = add onee y in
    let x5 = mul x4 x5 in

    if equal (mul x5 x) onee then
      x5
    else
      assert false
  in

  match size_of x with
  | 8 -> f8bits x
  | 16 -> f16bits x
  | 32 -> f32bits x
  | 64 -> f64bits x
  | 128 -> f128bits x
  | _ -> failwith ("Size not supported - " ^ (string_of_int (size_of x)))


let byteswap (bv: t) : t =
  let n = size_of bv in
  let rec loop i x =
    if i = n then x
    else
      let y = extract bv {lo=i; hi=i+8-1} in
      match x with
      | None -> loop (i+8) (Some y)
      | Some z -> loop (i+8) (Some (concat [z;y]))
  in
  loop 0 None
  |> Option.get


  let fold f init bv =
    let len = size_of bv in
    let rec loop acc i =
      if i = len then acc
      else loop (f acc (get_bit bv i)) (i+1)
    in
    loop init 0

let to_float bv  = Z.to_float (signed_of bv)
