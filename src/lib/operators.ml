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

(* Describes the diverse ops available (unop, binop, triop) *)

(* Dev notes : If you want to see the expansion of the ppx,
  use the following command :
  ``dune describe pp <path-to-source>``
*)

open Utility

type sizemod_t = 
  | Extend of int
  | Sextend of int
  | Reduce of int
  [@@deriving eq, ord]

type unop_t =
  | Minus
  | Not
  | Byteswap
  | Ehad
  | Arba
  | Shesh
  | Smol
  [@@deriving eq, enum, ord]

type binop_t =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | Xor
  | RShiftu
  | LShift
  | RShifts
  | RotateRight
  | Div_x86
  | SDiv_x86
  | Mod_x86
  | SMod_x86
  | RShiftu_x86
  | LShift_x86
  | RShifts_x86
  | UMin
  | SMin
  | UMax
  | SMax
  [@@deriving eq, enum, ord]

type triop_t = Div | SDiv | Mod | SMod | ITE | Im [@@deriving eq, enum, ord]

type 'a binop_side = Left of 'a | Right of 'a | Both of 'a
type 'a fromout = { from: 'a binop_side; out:'a }

let select omega = omega.(Random.int (Array.length omega))

module Extract (Op : sig 
  type t
  val number : int  
  val of_int : int -> t 
end) = struct
  let omega = Array.init Op.number Op.of_int

  let rand () =  select omega
  let cardinal () = Array.length omega
end

module SizeMod = struct
  type t = sizemod_t

  let eq = equal_sizemod_t
  
  let compare = compare_sizemod_t

  let apply t x = 
    match t with
    | Extend sz -> Bitvector.extend x sz
    | Sextend sz -> Bitvector.extend_signed x sz
    | Reduce sz -> Bitvector.reduce x sz

  let to_string = function
  | Extend sz -> Printf.sprintf "%dext" sz
  | Sextend sz -> Printf.sprintf "%dsext" sz
  | Reduce sz -> Printf.sprintf "%dred" sz

  let to_sygus op term1 sz1 = 
    match op with
    | Reduce sz -> Printf.sprintf "((_ extract %d 0) %s)" (sz-1) term1 
    | Extend sz -> Printf.sprintf "((_ zero_extend %d) %s)" (sz - sz1) term1
    | Sextend sz -> Printf.sprintf "((_ sign_extend %d) %s)" (sz - sz1) term1
end

module UnopBase = struct
  type t = unop_t
  let number = max_unop_t + 1 (* number of operators *)
  let compare = compare_unop_t

  let of_int t = unop_t_of_enum t |> Option.get
  let eq = equal_unop_t

  let of_string = function
  | "not" -> Some Not
  | "neg" -> Some Minus
  | "bswap" -> Some Byteswap
  | "ehad" -> Some Ehad
  | "arba" -> Some Arba
  | "shesh" -> Some Shesh
  | "smol" -> Some Smol
  | _ -> None

  let is_involutif = function
  | Minus | Not | Byteswap -> true
  | _ -> false

  let apply op x = match op with
  | Minus -> Bitvector.neg x
  | Not -> Bitvector.lognot x
  | Byteswap -> Bitvector.byteswap x
  | Ehad -> Bitvector.shift_right x 1
  | Arba -> Bitvector.shift_right x 4
  | Shesh -> Bitvector.shift_right x 16
  | Smol -> Bitvector.shift_left x 1

  let get_bitsz op bitsz = match op with
  | Minus | Not | Byteswap | Ehad | Arba | Shesh | Smol -> bitsz

  let to_string = function
  | Minus -> "-"
  | Not -> "not"
  | Byteswap -> "bswap"
  | Ehad -> "ehad"
  | Arba -> "arba"
  | Shesh -> "shesh"
  | Smol -> "smol"

  let to_sygus op term1 sz1 = 
    match op with
    | Minus -> Format.sprintf "(bvneg %s)" term1
    | Not -> Format.sprintf "(bvnot %s)" term1
    | Byteswap -> 
        let rec loop i =
            if i+8 == sz1 then
                Format.sprintf "((_ extract %d %d) %s)" (i+8-1) i term1
            else
                Format.sprintf "(concat ((_ extract %d %d) %s) %s)" (i+8-1) i term1 (loop (i+8))
        in
        loop 0
    | Ehad -> Format.sprintf "(bvlshr %s %s)" term1 (cst_to_smtlib 1 sz1)
    | Arba -> Format.sprintf "(bvlshr %s %s)" term1 (cst_to_smtlib 4 sz1)
    | Shesh -> Format.sprintf "(bvlshr %s %s)" term1 (cst_to_smtlib 16 sz1)
    | Smol -> Format.sprintf "(bvshl %s %s)" term1 (cst_to_smtlib 1 sz1)
end

module BinopBase = struct
  type t = binop_t

  let number = max_binop_t + 1 (* number of operators*)
  let compare = compare_binop_t


  let of_int t = binop_t_of_enum t |> Option.get

  let eq = equal_binop_t

  let of_string = function
  | "add" | "+" -> Some Add
  | "sub" | "-" -> Some Sub
  | "imul" | "*" -> Some Mul
  | "and" | "&" -> Some And
  | "or" | "|" -> Some Or
  | "xor" | "^" -> Some Xor
  | "rshiftu" | ">>" | ">>u" -> Some RShiftu
  | "lshift" | "<<" -> Some LShift
  | "rshifts" | ">>s" -> Some RShifts
  | "ror" -> Some RotateRight
  | "div_x86" -> Some Div_x86
  | "sdiv_x86" -> Some SDiv_x86
  | "mod_x86" -> Some Mod_x86
  | "smod_x86" -> Some SMod_x86
  | "rshiftu_x86" | ">>u86" -> Some RShiftu_x86
  | "lshift_x86" | "<<86" -> Some LShift_x86
  | "rshifts_x86" | ">>s86" -> Some RShifts_x86
  | "umin" -> Some UMin
  | "umax" -> Some UMax
  | "smin" -> Some SMin
  | "smax" -> Some SMax
  | _ -> None
  
  let is_commut = function
    | Add | Mul | And | Or | Xor | UMin | UMax | SMin | SMax -> true
    | _ -> false

  let is_assoc = function
    | Add | Mul | And | Or | Xor | UMin | UMax | SMin | SMax -> true
    | _ -> false

  let is_idempotent = function
    | And | Or | UMin | UMax | SMin | SMax -> true
    | _ -> false

  let is_nilpotent = function
    | Xor | Sub -> true
    | _ -> false
  
  let is_neutral_element ty ?(dir=Both()) e = 
    match ty, dir with
    | (Or | Add | Xor | UMax), _
      when Bitvector.is_zeros e -> true
    | (And | UMin), _ 
      when Bitvector.is_max_ubv e -> true 
    | SMax, _ 
      when Bitvector.is_min_sbv e -> true
    | SMin, _ 
      when Bitvector.is_max_sbv e -> true
    | Mul, _ 
      when Bitvector.is_ones e -> true
    | (Sub | RotateRight 
      | RShifts | RShiftu 
      | RShifts_x86 | RShiftu_x86 
      | LShift | LShift_x86), Right ()
      when Bitvector.is_zeros e -> true    
    | _ -> false
  
  let is_absorbing_element ty ?(dir=Both ()) e = 
    match ty, dir with
    | Or, _ -> Bitvector.is_max_ubv e
    | (Mul | And), _ -> Bitvector.is_zeros e
    | UMax, _ -> Bitvector.is_max_ubv e
    | UMin, _ -> Bitvector.is_zeros e
    | SMax, _ -> Bitvector.is_max_sbv e
    | SMin, _ -> Bitvector.is_min_sbv e
    | _ -> false

  let get_neutral op sz = 
    match op with
    | Or | Add | Xor | UMax -> Some (Both (Bitvector.zeros sz))
    | Sub | RotateRight 
    | RShifts | RShiftu 
    | RShifts_x86 | RShiftu_x86 
    | LShift | LShift_x86 -> Some (Right (Bitvector.zeros sz))
    | Mul -> Some (Both (Bitvector.ones sz))
    | And | UMin -> Some (Both (Bitvector.max_ubv sz)) 
    | SMax -> Some (Both (Bitvector.min_sbv sz))
    | SMin -> Some (Both (Bitvector.max_sbv sz))
    | _ -> None

  let has_neutral = function
  | Or | Add | Xor | UMax 
  | Sub | RotateRight 
  | RShifts | RShiftu 
  | RShifts_x86 | RShiftu_x86 
  | LShift | LShift_x86 
  | Mul | And  
  | SMax | SMin -> true
  | _ -> false

  let has_annihilator = function
  | Mul | And | Or
  | UMin | UMax
  | SMin | SMax
  | RShiftu | RShiftu_x86 
  | LShift | LShift_x86 -> true
  | _ -> false

  let get_annihilator op sz = match op with
  | Mul | And | UMin -> Some {from=(Both (Bitvector.zeros sz)); out=(Bitvector.zeros sz)}
  | Or | UMax -> Some {from=(Both (Bitvector.max_ubv sz)); out=(Bitvector.max_ubv sz)}
  | RShiftu | RShiftu_x86 
  | LShift | LShift_x86 -> Some {from=(Right (Bitvector.max_sbv sz)); out=(Bitvector.zeros sz)}
  | SMax -> Some {from=(Both (Bitvector.max_sbv sz)); out=(Bitvector.max_sbv sz)}
  | SMin -> Some {from=(Both (Bitvector.min_sbv sz)); out=(Bitvector.min_sbv sz)}
  | _ -> None

  let inversible = function
  | Add | Sub | Xor -> true
  | _ -> false

  let inverse op out cst = match op with
  | Add -> Some (Bitvector.sub out cst)
  | Sub -> Some (Bitvector.add out cst)
  | Xor -> Some (Bitvector.logxor out cst)
  | _ -> None

  let is_infix = function
  | Add | Sub | Mul 
  | And | Or | Xor 
  | RShiftu | LShift | RShifts
  | LShift_x86 | RShiftu_x86 | RShifts_x86
    -> true
  | RotateRight
  | Div_x86 | SDiv_x86 | Mod_x86 | SMod_x86
  | UMin | UMax | SMin | SMax 
    -> false

  let to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | And -> "&"
  | Or  -> "|"
  | Xor -> "^"
  | RShiftu -> ">>u"
  | LShift -> "<<"
  | RShifts -> ">>s"
  | RotateRight -> "ror"
  | Div_x86 -> "div_x86"
  | SDiv_x86 -> "sdiv_x86"
  | Mod_x86 -> "mod_x86"
  | SMod_x86 -> "smod_x86"
  | RShiftu_x86 -> ">>u86"
  | LShift_x86 -> "<<86"
  | RShifts_x86 -> ">>s86"
  | UMin -> "umin"
  | UMax -> "umax"
  | SMin -> "smin"
  | SMax -> "smax"

  let to_smtlib = function
  | Add -> "bvadd"
  | Sub -> "bvsub"
  | Mul -> "bvmul"
  | And -> "bvand"
  | Or  -> "bvor"
  | Xor -> "bvxor"
  | RShiftu -> "bvlshr"
  | LShift -> "bvshl"
  | RShifts -> "bvashr"
  | Div_x86 -> "bvudiv"
  | SDiv_x86 -> "bvsdiv"
  | Mod_x86 -> "bvurem"
  | SMod_x86 -> "bvsrem"
  | RShiftu_x86 -> "bvlshr"
  | LShift_x86 -> "bvshl"
  | RShifts_x86 -> "bvashr"
  | RotateRight -> assert false
  | UMin -> assert false
  | UMax -> assert false
  | SMin -> assert false
  | SMax -> assert false

  let to_sygus op term1 sz1 term2 sz2 = match op with
  | Add | Sub | Mul
  | And | Or | Xor 
  | RShiftu | LShift | RShifts
    -> Format.sprintf "(%s %s %s)" (to_smtlib op) term1 term2
  | LShift_x86 | RShifts_x86 | RShiftu_x86 -> (
      match sz1 with
          | 64 -> Format.sprintf "(%s %s (bvand %s #x000000000000003f ))" (to_smtlib op) term1 term2
          | 32 -> Format.sprintf "(%s %s (bvand %s #x0000001f ))" (to_smtlib op) term1 term2
          | 16 -> Format.sprintf "(%s %s (bvand %s #x001f ))" (to_smtlib op) term1 term2
          | 8 -> Format.sprintf "(%s %s (bvand %s #x1f ))" (to_smtlib op) term1 term2
          | _ -> failwith "Shift SMTLIB size not supported"
  )
  | Div_x86 | Mod_x86 ->
      Format.sprintf "((_ extract %d 0) (%s ((_ zero_extend %d) %s) ((_ zero_extend %d) %s)))" (sz1-1) (to_smtlib op) sz1 term1 sz1 term2    
  | SDiv_x86 | SMod_x86 ->
      Format.sprintf "((_ extract %d 0) (%s ((_ sign_extend %d) %s) ((_ sign_extend %d) %s)))" (sz1-1) (to_smtlib op) sz1 term1 sz1 term2 
  | RotateRight -> 
      let x = term1 in
      let ysize = cst_to_smtlib sz2 sz2 in
      let y = term2 in
      Format.sprintf "(bvor (bvlshr %s %s) (bvshl %s (bvsub %s %s)))" x y x ysize y 
  | UMin -> Format.sprintf "(ite (bvult %s %s) %s %s)" term1 term2 term1 term2
  | UMax -> Format.sprintf "(ite (bvugt %s %s) %s %s)" term1 term2 term1 term2
  | SMin -> Format.sprintf "(ite (bvslt %s %s) %s %s)" term1 term2 term1 term2
  | SMax -> Format.sprintf "(ite (bvsgt %s %s) %s %s)" term1 term2 term1 term2


  let apply op x y = 
      let _ = if (Bitvector.size_of x) != (Bitvector.size_of y) then failwith "Binop.apply: different sizes" in
      let size = Bitvector.size_of x in
      match op with
      | Add -> Bitvector.add x y
      | Sub -> Bitvector.sub x y
      | Mul -> Bitvector.mul x y
      | And -> Bitvector.logand x y
      | Or  -> Bitvector.logor x y
      | Xor -> Bitvector.logxor x y
      | RShiftu -> 
          if Bitvector.uge y (Bitvector.of_int ~size size) then (
            Bitvector.zeros size
          )
          else (
            let offset = Bitvector.to_uint y in
            Bitvector.shift_right x offset 
          )
      | LShift -> 
          if Bitvector.uge y (Bitvector.of_int ~size size) then (
            Bitvector.zeros size
          )
          else (
            let offset = Bitvector.to_uint y in
            Bitvector.shift_left x offset 
          )
      | RShifts -> 
          if Bitvector.uge y (Bitvector.of_int ~size size) then (
            if Bitvector.is_neg x then
              Bitvector.fill size
            else
              Bitvector.zeros size
          )
          else (
            let offset = Bitvector.to_uint y in 
            Bitvector.shift_right_signed x offset
          )
      | RotateRight ->
          let bvsize = Bitvector.of_int ~size:size size in
          let offset = Bitvector.(urem y bvsize |> to_uint) in
          Bitvector.rotate_right x offset

      | RShiftu_x86 ->
          let hex_mask = if Bitvector.size_of x = 64 then  "0x3f" else "0x1f" in
          let mask_and = Bitvector.extend (Bitvector.of_hexstring hex_mask) (Bitvector.size_of y) in
          let masked_y = Bitvector.to_int @@ Bitvector.logand y mask_and in
          let res = Bitvector.shift_right x masked_y in
          res

      | LShift_x86 -> 
          let hex_mask = if Bitvector.size_of x = 64 then  "0x3f" else "0x1f" in
          let mask_and = Bitvector.extend (Bitvector.of_hexstring hex_mask) (Bitvector.size_of y) in
          let masked_y = Bitvector.to_int @@ Bitvector.logand y mask_and in
          let res = Bitvector.shift_left x masked_y in
          res
          
      | RShifts_x86 -> 
          let hex_mask = if Bitvector.size_of x = 64 then  "0x3f" else "0x1f" in
          let mask_and = Bitvector.extend (Bitvector.of_hexstring hex_mask) (Bitvector.size_of y) in
          let masked_y = Bitvector.to_int @@ Bitvector.logand y mask_and in
          let res = Bitvector.shift_right_signed x masked_y in
          res

      | Div_x86 ->
          if Bitvector.is_zeros y then
              Bitvector.max_ubv size
          else
              let numerator_ext = Bitvector.extend x (2*size) in
              let denominator_ext = Bitvector.extend y (2*size) in
              let ret = Bitvector.udiv numerator_ext denominator_ext in
              Bitvector.reduce ret size

      | SDiv_x86 -> 
          let numerator_ext = Bitvector.extend_signed x (2*size) in
          if Bitvector.is_zeros y then
              if Bitvector.sge numerator_ext (Bitvector.zeros (2*size)) then
                  Bitvector.max_ubv size
              else
                  Bitvector.ones size
          else
              let denominator_ext = Bitvector.extend_signed y (2*size) in

              let ret = Bitvector.sdiv numerator_ext denominator_ext in
              Bitvector.reduce ret size

      | Mod_x86 ->
          let numerator_ext = Bitvector.extend x (2*size) in
          let divisor = Bitvector.extend y (2*size) in
          if Bitvector.is_zeros divisor then
              if Bitvector.equal numerator_ext divisor then
                  Bitvector.zeros size
              else x
          else
              Bitvector.reduce (Bitvector.umod numerator_ext divisor) size

      | SMod_x86 -> 
          let numerator_ext = Bitvector.extend_signed x (2*size) in
          let divisor = Bitvector.extend_signed y (2*size) in
          if Bitvector.is_zeros divisor then
              y
          else
              Bitvector.reduce (Bitvector.smod numerator_ext divisor) size

      | UMin -> Bitvector.umin x y
      | UMax -> Bitvector.umax x y
      | SMin -> Bitvector.smin x y
      | SMax -> Bitvector.smax x y

  let get_bitsz op bitsz1 bitsz2 =
    match op with
    | Add | Sub | Mul | And | Or | Xor | RShiftu | LShift | RShifts 
    | RotateRight | Div_x86 | SDiv_x86 | Mod_x86 | SMod_x86 | RShiftu_x86 
    | LShift_x86 | RShifts_x86 | UMin | SMin | UMax | SMax ->
      assert (bitsz1 == bitsz2);
      bitsz1
end

module TriopBase = struct
  type t = triop_t

  let number = max_triop_t + 1 (* number of operators *)
  let compare = compare_triop_t

  let of_int t = triop_t_of_enum t |> Option.get
  let eq = equal_triop_t

  let of_string = function
  | "div" | "/" -> Some Div
  | "sdiv" | "/s" -> Some SDiv
  | "umod" | "%" -> Some Mod
  | "smod" | "%s" -> Some SMod
  | "ite" -> Some ITE
  | "im" -> Some Im
  | _ -> None

  let to_string = function
  | Div -> "/"
  | SDiv -> "/s"
  | Mod -> "%"
  | SMod -> "%s"
  | ITE -> "if"
  | Im -> "im"

  let to_smtlib = function
  | Div -> "bvudiv"
  | SDiv -> "bvsdiv"
  | Mod -> "bvurem"
  | SMod -> "bvsrem"
  | ITE -> "ite"
  | Im -> assert false

  let to_sygus op term1 sz1 term2 sz2 term3 sz3 = 
    let catsize = sz1 + sz2 in
    let size = sz3 in
    match op with
    | Div | Mod -> 
      Format.sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ zero_extend %d) %s)))" (size-1) (to_smtlib op) term1 term2 (catsize - size) term3
    | SDiv | SMod -> 
      Format.sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ sign_extend %d) %s)))" (size-1) (to_smtlib op) term1 term2 (catsize - size) term3
    | ITE -> Format.sprintf "(ite (distinct  %s (_ bv0 %d)) %s %s)" term1 sz1 term2 term3
    | Im -> Format.sprintf "(ite (= %s %s) %s %s)" term1 (cst_to_smtlib 1 sz1) term2 term3

  let apply op x y z =
    let _ = if (Bitvector.size_of x) != (Bitvector.size_of y) || (Bitvector.size_of y) != (Bitvector.size_of z) then failwith "Triop.apply: different sizes" in
    let catsize = (Bitvector.size_of x) + (Bitvector.size_of y) in                                                                                                                                             
    let size = (Bitvector.size_of z) in
    match op with
    | Div ->
        if Bitvector.is_zeros z then
            Bitvector.max_ubv size
        else
            Bitvector.extract (Bitvector.udiv (Bitvector.concat [x; y]) (Bitvector.extend z catsize)) ~lo:0 ~hi:(size-1)
    | SDiv -> 
        let cat = Bitvector.concat [x; y] in
        if Bitvector.is_zeros z then
            if Bitvector.sge cat (Bitvector.zeros catsize) then
                Bitvector.max_ubv size
            else Bitvector.ones size
        else
            Bitvector.extract (Bitvector.sdiv cat (Bitvector.extend_signed z catsize)) ~lo:0 ~hi:(size-1)
    | Mod ->
        let cat = Bitvector.concat [x; y] in
        let divisor = Bitvector.extend z catsize in
        if Bitvector.is_zeros divisor then
            if Bitvector.equal cat divisor then
              Bitvector.zeros size
            else y
        else
            Bitvector.extract (Bitvector.umod cat divisor) ~lo:0 ~hi:(size-1)
    | SMod -> 
        let cat = Bitvector.concat [x; y] in
        let divisor = Bitvector.extend_signed z catsize in
        if Bitvector.is_zeros divisor then y
        else
          Bitvector.extract (Bitvector.smod cat divisor) ~lo:0 ~hi:(size-1)
    | ITE -> if Bitvector.is_zeros x then z else y
    | Im -> if Bitvector.is_ones x then y else z

  let get_bitsz op bitsz1 bitsz2 bitsz3 =
    match op with
    | Div | SDiv | Mod | SMod -> 
      assert (bitsz1 == bitsz2 && bitsz1 == bitsz3);
      bitsz1
    | ITE | Im -> 
      assert (bitsz2 == bitsz3);
      bitsz2
end

module Unop = struct
  include UnopBase
  include Extract (UnopBase)
end

module Binop = struct
  include BinopBase
  include Extract (BinopBase)
end

module Triop = struct
  include TriopBase
  include Extract (TriopBase)
end
