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

open Operators

type op_t =
  | OP_Var of Oracle.variable
  | OP_Const of Oracle.constant
  | OP_Unop of unop_t
  | OP_Binop of binop_t
  | OP_Triop of triop_t

type 'a node =
  | Var of Oracle.variable
  | Const of Oracle.constant
  | SizeMod of sizemod_t * 'a t
  | Unop of unop_t * 'a t
  | Binop of binop_t * 'a t * 'a t
  | Triop of triop_t * 'a t * 'a t * 'a t

and 'a t = {
  node : 'a node;
  (* number of nodes in this subtree, size modifiers do not count *)
  sz : int;
  (* bitsize of the values calculated by this tree *)
  bitsz : int;
  data : 'a;
}

let compare t1 t2 =
  let to_int = function
  | Const _ -> 0
  | Var _ -> 1
  | SizeMod _ -> 2
  | Unop _ -> 3
  | Binop _ -> 4
  | Triop _ -> 5
  in
  let rec aux t1 t2 =
    match t1.node, t2.node with
    | Var v1, Var v2 -> String.compare v1.name v2.name
    | Const c1, Const c2 -> Bitvector.compare c1.value c2.value
    | SizeMod (op1,t1'), SizeMod (op2, t2') -> (
      match SizeMod.compare op1 op2 with
      | 0 -> aux t1' t2'
      | x -> x)
    | Unop (op1,t1'), Unop (op2, t2') -> (
      match Unop.compare op1 op2 with
      | 0 -> aux t1' t2'
      | x -> x)
    | Binop (op1, t11, t12), Binop (op2, t21, t22) -> (
      match Binop.compare op1 op2 with
      | 0 -> (
        match aux t11 t21 with
        | 0 -> aux t12 t22
        | x -> x
        )
      | x -> x
    )
    | Triop (op1, t11, t12, t13), Triop (op2, t21, t22, t23) -> (
      match Triop.compare op1 op2 with
      | 0 -> (
        match aux t11 t21 with
        | 0 -> (
          match aux t12 t22 with
          | 0 -> aux t13 t23
          | x -> x
        )
        | x -> x
        )
      | x -> x
    )
    | _, _ -> Int.compare (to_int t1.node) (to_int t2.node)
  in
  aux t1 t2

let _fold_topdown (f : 'a t -> 'b -> 'b) (tree : 'a t) (x : 'b) =
  let rec aux t x =
    let x = f t x in
    match t.node with
    | Const _ | Var _ -> x
    | SizeMod (_, t1) -> aux t1 x
    | Unop (_, t1) -> aux t1 x
    | Binop (_, t1, t2) -> aux t2 (aux t1 x) 
    | Triop (_, t1, t2, t3) -> aux t3 (aux t2 (aux t1 x))
  in
  aux tree x

let _fold_bottomup (f : 'a t -> 'b -> 'b) (tree : 'a t) (x : 'b) =
  let rec aux t x =
    let y =
      match t.node with
      | Const _ | Var _ -> x
      | SizeMod (_, t1) -> aux t1 x
      | Unop (_, t1) -> aux t1 x
      | Binop (_, t1, t2) -> aux t2 (aux t1 x) 
      | Triop (_, t1, t2, t3) -> aux t3 (aux t2 (aux t1 x))
    in
    f t y
  in
  aux tree x

let rec tree_equal t1 t2 =
  if t1.sz != t2.sz then false
  else (
    match (t1.node, t2.node) with
    | Const c1, Const c2 when c1 = c2 -> true
    | Var v1, Var v2 when v1 = v2 -> true
    | Unop (op1, t1'), Unop (op2, t2') when op1 = op2 -> 
      tree_equal t1' t2'
    | Binop (op1, t11', t12'), Binop (op2, t21', t22') when op1 = op2 -> 
      if Binop.is_commut op1 then
        (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21')
      else
        tree_equal t11' t21' && tree_equal t12' t22'
    | Triop (op1, t11', t12', t13'), Triop (op2, t21', t22', t23') when op1 = op2 ->
        tree_equal t11' t21' && tree_equal t12' t22' && tree_equal t13' t23'
    | _, _ -> false
  )

let to_string f (t : 'a t) =
  let open Printf in
  let rec aux t =
    match t.node with
    | Var l -> sprintf "%s<%d>%s" l.name l.sz (f t.data)
    | Const c -> sprintf "%s%s" c.name (f t.data)
    | SizeMod (op, t) -> sprintf "%s (%s)%s" (SizeMod.to_string op) (aux t) (f t.data)
    | Unop (op, t) -> sprintf "%s (%s)%s" (Unop.to_string op) (aux t) (f t.data)
    | Binop (op, t1, t2) ->
        sprintf "(%s %s %s)%s" (aux t1) (Binop.to_string op) (aux t2) (f t.data)
    | Triop (op, t1, t2, t3) -> (
        match op with
        | Div | SDiv | Mod | SMod ->
            sprintf "((%s ++ %s) %s %s)%s" (aux t1) (aux t2)
              (Triop.to_string op) (aux t3) (f t.data)
        | ITE | Im ->
            sprintf "%s(%s, %s, %s)%s" (Triop.to_string op) (aux t1) (aux t2)
              (aux t3) (f t.data))
  in
  aux t

let mk_var data var =
  { 
    node = Var var;
    sz = 1;
    bitsz = var.sz;
    data 
  }

let mk_const data c =
  { 
    node = Const c;
    sz = 1;
    bitsz = Bitvector.size_of c.value;
    data 
  }

let mk_const_of_bv data bv =
  mk_const data { name = Bitvector.to_string bv; value = bv }

let mk_sizemod data op t =
  let sz, bitsz =
    match op with
    | Sextend x | Extend x | Reduce x -> (t.sz, x)
  in
  { 
    node = SizeMod (op, t);
    sz;
    bitsz;
    data
  }
let mk_unop data op t =
  let sz, bitsz = (t.sz + 1, t.bitsz)
  in
  { 
    node = Unop (op, t);
    sz;
    bitsz;
    data
  }

let mk_binop data op t1 t2 =
  { 
    node = Binop (op, t1, t2); 
    sz = 1 + t1.sz + t2.sz; 
    bitsz = t1.bitsz; 
    data
  }

let mk_triop data op t1 t2 t3 =
  {
    node = Triop (op, t1, t2, t3);
    sz = 1 + t1.sz + t2.sz + t3.sz;
    bitsz = t2.bitsz;
    (* NB: We should check that t2.bitsz == t3.bitsz in the ITE and Im case *)
    data;
  }

module type TreeConstructor = sig
  type data
  type t' := data t 

  (* Specialized Constructor *)
  val mk_var : Oracle.variable -> t'
  val mk_const : Oracle.constant -> t'
  val mk_const_of_bv : Bitvector.t -> t'
  val mk_sizemod : sizemod_t -> t' -> t'
  val mk_unop : unop_t -> t' -> t'
  val mk_binop : binop_t -> t' -> t' -> t'
  val mk_triop : triop_t -> t' -> t' -> t' -> t'

  val eq : t' -> t' -> bool
  val compare : t' -> t' -> int

  val transfer : 'a t -> t'
end

module UnitTreeConstructor = struct
  type data = unit

  module H = Hashtbl

  let table = H.create 251

  let hashcons x =
    try H.find table x
    with Not_found ->
      H.add table x x;
      x
  
  (* A specialized version that handles commutative
     operators (which helps to ensure maximal sharing) *)
  let hashcons_binop x =
  match x.node with
  | Binop (op, t1, t2) when Operators.Binop.is_commut op -> (
      try H.find table x
      with Not_found -> (
        let x' = mk_binop () op t2 t1 in
        try H.find table x'
        with Not_found ->
          H.add table x x;
          x))
  | _ -> hashcons x

  (* Specialized Constructor *)
  let mk_var s = hashcons (mk_var () s)
  let mk_const c = hashcons (mk_const () c)
  let mk_const_of_bv bv = hashcons (mk_const_of_bv () bv)
  let mk_sizemod op t = hashcons (mk_sizemod () op t)
  let mk_unop op t = hashcons (mk_unop () op t)
  let mk_binop op t1 t2 = hashcons_binop (mk_binop () op t1 t2)
  let mk_triop op t1 t2 t3 = hashcons (mk_triop () op t1 t2 t3)

  let eq = (==)

  (* Optimized version using physical equality to short-cut
     when possible *)
  let compare t1 t2 =
    let to_int = function
    | Const _ -> 0
    | Var _ -> 1
    | SizeMod _ -> 2
    | Unop _ -> 3
    | Binop _ -> 4
    | Triop _ -> 5
    in
  let rec aux t1 t2 =
    if t1 == t2 then 0
    else (
      match t1.node, t2.node with
      | Var v1, Var v2 -> String.compare v1.name v2.name
      | Const c1, Const c2 -> Bitvector.compare c1.value c2.value
      | SizeMod (op1,t1'), SizeMod (op2, t2') -> (
        match SizeMod.compare op1 op2 with
        | 0 -> aux t1' t2'
        | x -> x)
      | Unop (op1,t1'), Unop (op2, t2') -> (
        match Unop.compare op1 op2 with
        | 0 -> aux t1' t2'
        | x -> x)
      | Binop (op1, t11, t12), Binop (op2, t21, t22) -> (
        match Binop.compare op1 op2 with
        | 0 -> (
          match aux t11 t21 with
          | 0 -> aux t12 t22
          | x -> x
          )
        | x -> x
      )
      | Triop (op1, t11, t12, t13), Triop (op2, t21, t22, t23) -> (
        match Triop.compare op1 op2 with
        | 0 -> (
          match aux t11 t21 with
          | 0 -> (
            match aux t12 t22 with
            | 0 -> aux t13 t23
            | x -> x
          )
          | x -> x
          )
        | x -> x
      )
      | _, _ -> Int.compare (to_int t1.node) (to_int t2.node)
    )
  in
  aux t1 t2

  let rec transfer t =
    match t.node with
    | Var s -> mk_var s
    | Const bv -> mk_const bv
    | SizeMod (op,t) -> 
      mk_sizemod op (transfer t)
    | Unop (op,t) ->
      mk_unop op (transfer t)
    | Binop (op,t1,t2) ->
      mk_binop op (transfer t1) (transfer t2)
    | Triop (op,t1,t2,t3) ->
      mk_triop op (transfer t1) (transfer t2) (transfer t3)
end