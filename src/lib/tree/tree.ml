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

type variable = { name : string ; sz : int }
type constant = { name : string ; value : Bitvector.t }

type op_t =
  | OP_Var of variable
  | OP_Const of constant
  | OP_Unop of unop_t
  | OP_Binop of binop_t
  | OP_Triop of triop_t

type 'a node =
  | Var of variable
  | Const of constant
  | SizeMod of sizemod_t * 'a t
  | Unop of unop_t * 'a t
  | Binop of binop_t * 'a t * 'a t
  | Triop of triop_t * 'a t * 'a t * 'a t

and 'a t = {
  node : 'a node;
  (* Number of nodes with size modifiers *)
  sz : int;
  (* number of nodes in this subtree, size modifiers do not count *)
  sz_no_mod : int;
  (* bitsize of the values calculated by this tree *)
  bitsz : int;
  data : 'a;
}

let eq_var (v1 : variable) (v2 : variable) =
  String.equal v1.name v2.name

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
    let if_eq t1' t2' cmp =
      match cmp with | 0 -> aux t1' t2' | _ -> cmp
    in
    match t1.node, t2.node with
    | Var v1, Var v2 -> String.compare v1.name v2.name
    | Const c1, Const c2 -> Bitvector.compare c1.value c2.value
    | SizeMod (op1,t1'), SizeMod (op2, t2') -> (
        SizeMod.compare op1 op2
        |> if_eq t1' t2'
      )
    | Unop (op1,t1'), Unop (op2, t2') -> (
        Unop.compare op1 op2
        |> if_eq t1' t2'
      )
    | Binop (op1, t11, t12), Binop (op2, t21, t22) -> (
        Binop.compare op1 op2
        |> if_eq t11 t21
        |> if_eq t12 t22
    )
    | Triop (op1, t11, t12, t13), Triop (op2, t21, t22, t23) -> (
        Triop.compare op1 op2
        |> if_eq t11 t21
        |> if_eq t12 t22
        |> if_eq t13 t23
    )
    | _, _ -> Int.compare (to_int t1.node) (to_int t2.node)
  in
  aux t1 t2

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
        if Binop.is_infix op then
          sprintf "(%s %s %s)%s" (aux t1) (Binop.to_string op) (aux t2) (f t.data)
        else
          sprintf "(%s %s %s)%s" (Binop.to_string op) (aux t1) (aux t2) (f t.data) 
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

let string_of t = to_string (fun _ -> "") t 

let var_of name sz : variable = { name; sz}

let const_of_bitv bv = 
  let name = Bitvector.to_string bv in
  {name; value=bv}

let const_of_int value size = 
  let bv = Bitvector.of_int ~size value in
  const_of_bitv bv

let get_vars t =
  let res = Hashtbl.create 0 in 
  let rec aux t = match t.node with
  | Var v -> Hashtbl.add res v ()
  | Const _ -> ()
  | SizeMod (_, t) | Unop (_, t) -> aux t
  | Binop (_, t1, t2) -> aux t1; aux t2
  | Triop (_, t1, t2, t3) -> aux t1; aux t2; aux t3
  in
  aux t;
  Hashtbl.fold (fun key _ acc -> key::acc) res []

let mk_var data var =
  { 
    node = Var var;
    sz = 1;
    sz_no_mod = 1;
    bitsz = var.sz;
    data 
  }

let mk_const data c =
  { 
    node = Const c;
    sz = 1;
    sz_no_mod = 1;
    bitsz = Bitvector.size_of c.value;
    data 
  }

let mk_const_of_bv data bv =
  mk_const data { name = Bitvector.to_string bv; value = bv }

let mk_sizemod data op t =
  let bitsz =
    match op with
    | Sextend x | Extend x | Reduce x -> x
  in
  { 
    node = SizeMod (op, t);
    sz = t.sz + 1;
    sz_no_mod = t.sz_no_mod;
    bitsz;
    data
  }
let mk_unop data op t =
  { 
    node = Unop (op, t);
    sz_no_mod = t.sz_no_mod + 1;
    sz = t.sz + 1;
    bitsz = Unop.get_bitsz op t.bitsz;
    data
  }

let mk_binop data op t1 t2 =
  { 
    node = Binop (op, t1, t2); 
    sz = 1 + t1.sz + t2.sz; 
    sz_no_mod = 1 + t1.sz_no_mod + t2.sz_no_mod; 
    bitsz = Binop.get_bitsz op t1.bitsz t2.bitsz;
    data
  }

let mk_triop data op t1 t2 t3 =
  {
    node = Triop (op, t1, t2, t3);
    sz = 1 + t1.sz + t2.sz + t3.sz;
    sz_no_mod = 1 + t1.sz_no_mod + t2.sz_no_mod + t3.sz_no_mod;
    bitsz = Triop.get_bitsz op t1.bitsz t2.bitsz t3.bitsz;
    data;
  }

(* Replace var in t by repl *)
let replace_var t (var: variable) repl =  
  let rec aux t = match t.node with
  | Var v -> 
    if String.equal var.name v.name then repl
    else t
  | Const _ -> t
  | SizeMod (op, t1) -> mk_sizemod t.data op (aux t1)  
  | Unop (op, t1) -> mk_unop t.data op (aux t1) 
  | Binop (op, t1, t2) -> mk_binop t.data op (aux t1) (aux t2) 
  | Triop (op, t1, t2, t3) -> mk_triop t.data op (aux t1) (aux t2) (aux t3)
  in
  aux t

module type TreeConstructor = sig
  type data
  type t' := data t 

  (* Specialized Constructor *)
  val mk_var : variable -> t'
  val mk_const :constant -> t'
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
    let if_eq t1' t2' cmp =
      match cmp with | 0 -> aux t1' t2' | _ -> cmp
    in
    if t1 == t2 then 0
    else (
      match t1.node, t2.node with
      | Var v1, Var v2 -> String.compare v1.name v2.name
      | Const c1, Const c2 -> Bitvector.compare c1.value c2.value
      | SizeMod (op1,t1'), SizeMod (op2, t2') -> (
        SizeMod.compare op1 op2
        |> if_eq t1' t2'
      )
      | Unop (op1,t1'), Unop (op2, t2') -> (
        Unop.compare op1 op2
        |> if_eq t1' t2'
      )
      | Binop (op1, t11, t12), Binop (op2, t21, t22) -> (
        Binop.compare op1 op2
        |> if_eq t11 t21
        |> if_eq t12 t22
      )
      | Triop (op1, t11, t12, t13), Triop (op2, t21, t22, t23) -> (
        Triop.compare op1 op2
        |> if_eq t11 t21
        |> if_eq t12 t22
        |> if_eq t13 t23
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
