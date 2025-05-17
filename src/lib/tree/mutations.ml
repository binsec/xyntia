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

open Tree
open Operators

module type MUTATION_BASE = sig
  val spe_ops : op_t array
end

module type MUTATIONS = sig
  val all_mut : unit -> op_t array
  val unop_mut : unit -> op_t array
  val binop_mut : unit -> op_t array
  val triop_mut : unit -> op_t array

  val unop_sygus : op_t -> string -> int -> string
  val binop_sygus : op_t -> string -> int -> string -> int -> string
  val triop_sygus :
    op_t -> string -> int -> string -> int -> string -> int -> string
end

module Mk_Mutations (M : MUTATION_BASE) (O : Oracle.ORACLE) : MUTATIONS = struct
  let all_mut () =
    let consts = Array.map (fun x -> OP_Const x) (O.consts ()) in
    let vars = Array.map (fun x -> OP_Var x) (O.vars ()) in
    Array.concat [ consts; vars; M.spe_ops ]

  let unop_mut () =
    Array.to_list (all_mut ())
    |> List.filter (function
         | OP_Const _ | OP_Var _ | OP_Unop _ -> true
         | _ -> false)
    |> Array.of_list

  let binop_mut () =
    Array.to_list (all_mut ())
    |> List.filter (function
         | OP_Const _ | OP_Var _ | OP_Binop _ -> true
         | _ -> false)
    |> Array.of_list

  let triop_mut () =
    Array.to_list (all_mut ())
    |> List.filter (function
         | OP_Const _ | OP_Var _ | OP_Triop _ -> true
         | _ -> false)
    |> Array.of_list

  let unop_sygus op nonterm1 sz1 =
    match op with
    | OP_Const c ->
        let z = Bitvector.signed_of c.value in
        let bv = Bitvector.create z sz1 in
        Utility.bv_to_smtlib bv
    | OP_Var v -> if v.sz == sz1 then v.name else ""
    | OP_Unop unop -> Unop.to_sygus unop nonterm1 sz1
    | _ -> failwith "not a unop"

  let binop_sygus op nonterm1 sz1 nonterm2 sz2 =
    match op with
    | OP_Const _ -> ""
    | OP_Var _ -> ""
    | OP_Binop binop -> Binop.to_sygus binop nonterm1 sz1 nonterm2 sz2
    | _ -> failwith "not a binop"

  let triop_sygus op nonterm1 sz1 nonterm2 sz2 nonterm3 sz3 =
    match op with
    | OP_Const _ -> ""
    | OP_Var _ -> ""
    | OP_Triop triop ->
        Triop.to_sygus triop nonterm1 sz1 nonterm2 sz2 nonterm3 sz3
    | _ -> failwith "not a triop"
end

module Mk_Mutations_Five(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Mul);
      OP_Binop(Or);
    |] 
  end) (O)

module Mk_Mutations_Mba(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor)
    |] 
  end) (O)

module Mk_Mutations_Mba_Shift(O : Oracle.ORACLE) : MUTATIONS =
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);
      OP_Binop(RShiftu);
      OP_Binop(RShifts);
      OP_Binop(LShift);
    |] 
  end) (O)

module Mk_Mutations_Mba_ITE(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);

      OP_Triop(ITE)
    |] 
  end) (O)

module Mk_Mutations_Expr(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);

      OP_Triop(Div);
      OP_Triop(SDiv)
    |] 
  end) (O)

module Mk_Mutations_Expr_ITE(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);

      OP_Triop(Div);
      OP_Triop(SDiv);

      OP_Triop(ITE)
    |] 
  end) (O)

module Mk_Mutations_Expr_x86(O : Oracle.ORACLE) : MUTATIONS =
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);

      OP_Binop(Div_x86);
      OP_Binop(SDiv_x86);
    |] 
  end) (O)

module Mk_Mutations_Full(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);
      OP_Binop(RShiftu);
      OP_Binop(RShifts);
      OP_Binop(LShift);

      OP_Triop(Div);
      OP_Triop(SDiv);
      OP_Triop(Mod);
      OP_Triop(SMod)
    |] 
  end) (O)

module Mk_Mutations_Full_x86(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Minus);
      OP_Unop(Not);

      OP_Binop(Add);
      OP_Binop(Sub);
      OP_Binop(Mul);
      OP_Binop(And);
      OP_Binop(Or);
      OP_Binop(Xor);

      OP_Binop(RShiftu_x86);
      OP_Binop(RShifts_x86);
      OP_Binop(LShift_x86);

      OP_Binop(Div_x86);
      OP_Binop(SDiv_x86);
      OP_Binop(Mod_x86);
      OP_Binop(SMod_x86)
    |] 
  end) (O)

module Mk_Mutations_Sygus(O : Oracle.ORACLE) : MUTATIONS = 
  Mk_Mutations(struct 
    let spe_ops = [|
      OP_Unop(Ehad);
      OP_Unop(Arba);
      OP_Unop(Shesh);
      OP_Unop(Smol);
      OP_Triop(Im);
            
      OP_Unop Not;

      OP_Binop And;
      OP_Binop Or;
      OP_Binop Xor;
      OP_Binop Add;
    |]
  end) (O)

let mutations_of_str (module O : Oracle.ORACLE) str =
  match str with
  | "5" -> (module Mk_Mutations_Five (O) : MUTATIONS)
  | "mba" -> (module Mk_Mutations_Mba (O) : MUTATIONS)
  | "expr" -> (module Mk_Mutations_Expr (O) : MUTATIONS)
  | "expr_ite" -> (module Mk_Mutations_Expr_ITE (O) : MUTATIONS)
  | "mba_shift" -> (module Mk_Mutations_Mba_Shift (O) : MUTATIONS)
  | "mba_ite" -> (module Mk_Mutations_Mba_ITE (O) : MUTATIONS)
  | "full" -> (module Mk_Mutations_Full (O) : MUTATIONS)
  | "sygus" -> (module Mk_Mutations_Sygus (O) : MUTATIONS)
  | "expr_x86" -> (module Mk_Mutations_Expr_x86 (O) : MUTATIONS)
  | "full_x86" -> (module Mk_Mutations_Full_x86 (O) : MUTATIONS)
  | _ -> assert false

(* Create the set of mutations from a user given list of operator's string representation *)
let mutations_of_operators (module O : Oracle.ORACLE) arr =
  let muts = Array.map (fun s ->
    match Unop.of_string s with
    | Some ty -> OP_Unop ty
    | None -> (
      match Binop.of_string s with
      | Some ty -> OP_Binop ty
      | None -> (
        match Triop.of_string s with
        | Some ty -> OP_Triop ty
        | None -> failwith "Unknow operator"
      )
    )) arr
  in
  (module Mk_Mutations (struct
    let spe_ops = muts
  end) (O) : MUTATIONS)

type mutd = {
  mutable last_mutation_indice : int option;
  all_choices : op_t array;
  mutable left_choices : op_t array;
  vals : Bitvector.t array;
}

module type MUTATOR = sig
  include TreeConstructor with type data = mutd
  type t := mutd t

  val singleton : unit -> t
  val mutate : int -> t -> int option -> t
  val mutate_mh : int -> t -> int option -> t * float
  val cut : t -> t
  val eval : t -> Bitvector.t array
  val to_string : t -> string
  val to_smtlib : t -> string

  val get_expr_size : t -> int
  val get_mutations : unit -> (module MUTATIONS)
end

module Mk_Mutator (O : Oracle.ORACLE) (M : MUTATIONS) : MUTATOR = struct
  module Unop = Mk_Unop (O)
  module Binop = Mk_Binop (O)
  module Triop = Mk_Triop (O)
  include M

  type data = mutd

  exception NOCHOICE

  let mk_var var =
    let data = {
      vals = O.var_values var;
      last_mutation_indice = None;
      all_choices = all_mut ();
      left_choices = all_mut ();
    }
    in
    mk_var data var

  let mk_const c =
    let data = {
      vals = O.const_values c;
      last_mutation_indice = None;
      all_choices = all_mut ();
      left_choices = all_mut ();
    }
    in
    mk_const data c

  let mk_const_of_bv bv = mk_const (O.const_of_bitv bv)

  let mk_sizemod op t =
    let data = {
      vals = Array.map (fun x -> SizeMod.apply op x) t.data.vals;
      last_mutation_indice = None;
      all_choices = [||];
      left_choices = [||];
    }
    in
    mk_sizemod data op t

  let mk_unop op t =
    let data = {
      vals = Array.map (fun x -> Unop.apply op x) t.data.vals;
      last_mutation_indice = None;
      all_choices = unop_mut ();
      left_choices = unop_mut ();
    }
    in
    mk_unop data op t

  let mk_binop op t1 t2 =
    let data = {
      vals = Array.map2 (fun x y -> Binop.apply op x y) t1.data.vals t2.data.vals;
      last_mutation_indice = None;
      all_choices = binop_mut ();
      left_choices = binop_mut ();
    }
    in
    mk_binop data op t1 t2

  let mk_triop op t1 t2 t3 =
    let data = {
      vals = Utility.map3 (fun x y z -> Triop.apply op x y z)
             t1.data.vals t2.data.vals t3.data.vals;
      last_mutation_indice = None;
      all_choices = triop_mut ();
      left_choices = triop_mut ();
    }
    in
    mk_triop data op t1 t2 t3
  
  let eq t1 t2 = Tree.tree_equal t1 t2
  let compare t1 t2 = Tree.compare t1 t2

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

  let mk_terminal bitsz leaf =
    if leaf.bitsz = bitsz then leaf
    else if leaf.bitsz < bitsz && Random.bool () then
      mk_sizemod (Extend bitsz) leaf
    else if leaf.bitsz < bitsz then 
      mk_sizemod (Sextend bitsz) leaf
    else mk_sizemod (Reduce bitsz) leaf

  let get_random_left_choice (t : mutd) indice =
    let choices =
      match (t.last_mutation_indice, indice) with
      | Some li, Some i ->
          if li < i then
            let _ = t.last_mutation_indice <- indice in
            t.all_choices
          else t.left_choices
      | None, Some _ ->
          let _ = t.last_mutation_indice <- indice in
          t.all_choices
      | Some _, None -> assert false
      | None, None -> t.all_choices
    in
    let size = Array.length choices in
    if size = 0 then None
    else
      let value = Random.int size in
      let res = choices.(value) in
      if Option.is_some indice then
        t.left_choices <- Array.init (size - 1) (fun i -> if i < value then choices.(i) else choices.(i + 1));
      Some res

  let random_terminal (bitsz : int) : mutd t =
    let nvars = O.nvars () in
    let nconsts = O.nconsts () in
    let choice = Random.int (nvars + nconsts) in
    if choice < nvars then mk_terminal bitsz (mk_var (O.random_var ()))
    else mk_terminal bitsz (mk_const (O.random_const ()))

  let terminal_from_const c (bitsz : int) = mk_terminal bitsz (mk_const c)
  let terminal_from_var v (bitsz : int) = mk_terminal bitsz (mk_var v)
    
  let random_unop ?op bitsz =
    match op with
    | Some operator -> mk_unop operator (random_terminal bitsz)
    | None -> mk_unop (Unop.rand ()) (random_terminal bitsz)

  let random_binop ?op bitsz =
    match op with
    | Some operator ->
        mk_binop operator 
          (random_terminal bitsz) 
          (random_terminal bitsz)
    | None ->
        mk_binop (Binop.rand ()) 
          (random_terminal bitsz)
          (random_terminal bitsz)

  let random_triop ?op bitsz =
    match op with
    | Some operator ->
        mk_triop operator 
          (random_terminal bitsz)
          (random_terminal bitsz)
          (random_terminal bitsz)
    | None ->
        mk_triop (Triop.rand ())
          (random_terminal bitsz)
          (random_terminal bitsz)
          (random_terminal bitsz)

  let rec is_terminal { node; _ } =
    match node with
    | Var _ | Const _ -> true
    | SizeMod (_, t') -> is_terminal t'
    | _ -> false

  let mutate height_lim t indice =
    let rec aux d id t =
      match t.node with
      | Var _ | Const _ -> mutate_leaf d t
      | SizeMod (_, t') -> 
        if is_terminal t then mutate_leaf d t
        else aux d id t'
      | Unop (op, t') ->
          if is_terminal t then mutate_leaf d t
          else if id = t.sz then mutate_unop t
          else
            let tree = aux (d + 1) id t' in
            mk_unop op tree
      | Binop (op, t1, t2) ->
          let rank = 1 + t1.sz in
          if id = rank then mutate_binop t
          else if id < rank then
            let tree = aux (d + 1) id t1 in
            mk_binop op tree t2
          else
            let tree = aux (d + 1) (id - rank) t2 in
            mk_binop op t1 tree
      | Triop (op, t1, t2, t3) ->
          let rank = 1 + t1.sz in
          if id = rank then mutate_triop t
          else if id < rank then
            let tree = aux (d + 1) id t1 in
            mk_triop op tree t2 t3
          else if id > rank + t2.sz then
            let tree = aux (d + 1) (id - (rank + t2.sz)) t3 in
            mk_triop op t1 t2 tree
          else
            let tree = aux (d + 1) (id - rank) t2 in
            mk_triop op t1 tree t3
    and mutate_leaf d t =
      if d = height_lim then random_terminal t.bitsz
      else
        let choice =
          match get_random_left_choice t.data indice with
          | Some v -> v
          | None -> raise NOCHOICE
        in
        match choice with
        | OP_Const c -> terminal_from_const c t.bitsz
        | OP_Var v -> terminal_from_var v t.bitsz
        | OP_Unop op -> random_unop ~op t.bitsz
        | OP_Binop op -> random_binop ~op t.bitsz
        | OP_Triop op -> random_triop ~op t.bitsz
    and mutate_unop t =
      match t.node with
      | Unop (_, t') -> (
          let choice =
            match get_random_left_choice t.data indice with
            | Some v -> v
            | None -> raise NOCHOICE
          in
          match choice with
          | OP_Const c -> terminal_from_const c t.bitsz
          | OP_Var v -> terminal_from_var v t.bitsz
          | OP_Unop op -> mk_unop op t'
          | _ -> assert false)
      | _ -> assert false
    and mutate_binop t =
      match t.node with
      | Binop (_, t1, t2) -> (
          let choice =
            match get_random_left_choice t.data indice with
            | Some v -> v
            | None -> raise NOCHOICE
          in
          match choice with
          | OP_Const c -> terminal_from_const c t.bitsz
          | OP_Var v -> terminal_from_var v t.bitsz
          | OP_Binop op -> mk_binop op t1 t2
          | _ -> assert false)
      | _ -> assert false
    and mutate_triop t =
      match t.node with
      | Triop (_, t1, t2, t3) -> (
          let choice =
            match get_random_left_choice t.data indice with
            | Some v -> v
            | None -> raise NOCHOICE
          in
          match choice with
          | OP_Const c -> terminal_from_const c t.bitsz
          | OP_Var v -> terminal_from_var v t.bitsz
          | OP_Triop op -> mk_triop op t1 t2 t3
          | _ -> assert false)
      | _ -> assert false
    in

    let rec recmutate t indice max allowed_nodes =
      assert (max >= 0);
      let index = Random.int (Array.length allowed_nodes) in
      let rand_node = allowed_nodes.(index) in
      try aux 0 rand_node t
      with NOCHOICE ->
        if max = 0 || Array.length allowed_nodes = 1 then t
        else
          let new_allowed =
            Array.init
              (Array.length allowed_nodes - 1)
              (fun i -> if i < index then allowed_nodes.(i) else allowed_nodes.(i + 1))
          in
          recmutate t indice (max - 1) new_allowed
    in
    let allowed_nodes = Array.init t.sz (( + ) 1) in
    recmutate t indice t.sz allowed_nodes

  let cut t =
    let rec aux id t =
      match t.node with
      | Var _ | Const _ -> random_terminal t.bitsz
      | SizeMod (op, t') ->
          if id = t.sz then random_terminal t.bitsz else mk_sizemod op (aux id t')
      | Unop (op, t') ->
          if id = t.sz then random_terminal t.bitsz else mk_unop op (aux id t')
      | Binop (op, t1, t2) ->
          let rank = 1 + t1.sz in
          if id = rank then random_terminal t.bitsz
          else if id < rank then mk_binop op (aux id t1) t2
          else mk_binop op t1 (aux (id - rank) t2)
      | Triop (op, t1, t2, t3) ->
          let rank = 1 + t1.sz in
          if id = rank then random_terminal t.bitsz
          else if id < rank then mk_triop op (aux id t1) t2 t3
          else if id > rank + t2.sz then
            mk_triop op t1 t2 (aux (id - (rank + t2.sz)) t3)
          else mk_triop op t1 (aux (id - rank) t2) t3
    in
    aux (1 + Random.int t.sz) t

  let eval t = t.data.vals

  let singleton () : mutd t =
    let { sz; _ } : Oracle.variable = O.out_var () in
    random_terminal sz

    let to_string (t : mutd t) =
      to_string (fun _ -> "") t

    let to_smtlib (t : mutd t) =
        let open Printf in
        let rec aux t = 
            match t.node with
            | Var l -> l.name
            | Const c -> 
                let hexstr = (Bitvector.to_hexstring c.value) in
                sprintf "#x%s"  (String.sub hexstr 2 ((String.length hexstr) -2))
            | SizeMod (op, t) ->
                SizeMod.to_sygus op (aux t) t.bitsz
            | Unop (op, t) ->
                Unop.to_sygus op (aux t) t.bitsz
            | Binop (op, t1, t2) ->
                Binop.to_sygus op (aux t1) t1.bitsz (aux t2) t2.bitsz
            | Triop (op, t1, t2, t3) ->
                Triop.to_sygus op (aux t1) t1.bitsz (aux t2) t2.bitsz (aux t3) t3.bitsz
        in
        aux t

  let mutate_mh height_lim t _ =
    let num_terminals = O.nconsts () + O.nvars () in
    let inv x = 1. /. x in
    let leaf_unop = 1.5 *. float (Unop.cardinal ()) in
    let unop_leaf = inv leaf_unop in
    let leaf_binop = 1.5 *. float (Binop.cardinal () * num_terminals) in
    let binop_leaf = inv leaf_binop in
    let leaf_triop = 1.5 *. float (Triop.cardinal () * num_terminals) in
    assert (leaf_triop = 0.0);
    let rec aux d id t =
      match t.node with
      | Var _ | Const _ -> mutate_leaf d t
      | SizeMod (_, t') ->
        if is_terminal t then mutate_leaf d t
        else aux d id t'
      | Unop (op, t') ->
          if is_terminal t then mutate_leaf d t
          else if id = t.sz then mutate_unop t
          else
            let t', r = aux (d + 1) id t' in
            (mk_unop op t', r)
      | Binop (op, t1, t2) ->
          let rank = 1 + t1.sz in
          if id = rank then mutate_binop t
          else if id < rank then
            let t1', r = aux (d + 1) id t1 in
            (mk_binop op t1' t2, r)
          else
            let t2', r = aux (d + 1) (id - rank) t2 in
            (mk_binop op t1 t2', r)
      | Triop (_, _, _, _) -> assert false
    and mutate_leaf d t =
      let choice = Random.int 3 in
      if d = height_lim || choice = 0 then (random_terminal t.bitsz, 1.)
      else if choice = 1 then (random_unop t.bitsz, leaf_unop)
      else (random_binop t.bitsz, leaf_binop)
    and mutate_unop t =
      match t.node with
      | Unop (_, t') ->
          if is_terminal t' && Random.bool () then
            (random_terminal t.bitsz, unop_leaf)
          else (mk_unop (Unop.rand ()) t', 1.)
      | _ -> assert false
    and mutate_binop t =
      match t.node with
      | Binop (_, t1, t2) ->
          if is_terminal t1 && is_terminal t2 && Random.bool () then
            (random_terminal t.bitsz, binop_leaf)
          else (mk_binop (Binop.rand ()) t1 t2, 1.)
      | _ -> assert false
    in

    let t', r = aux 0 (1 + Random.int t.sz) t in
    (t', r *. float t.sz /. float t'.sz)

  let get_expr_size t = t.sz
  let get_mutations () = (module M : MUTATIONS)
end