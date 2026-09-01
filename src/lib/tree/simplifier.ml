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

open Tree
open Operators

(**
  The [Pattern] module provides the tools to perform
  a match and rewrite algorithm on the trees as well
  as some helper functions / syntaxic sugar to ease
  its usage
*)
module Pattern = struct
  [@warning "-32"] 
  (* Ignore the warning of unused function for this module *)
  module C = UnitTreeConstructor (* This constructor uses hashconsing, allowing very fast tree equality check *)


  type tree = C.data Tree.t
  type const_id = int

  (* 
    Pattern-Rewriter type 
    Mirror the tree type with patrew instead of trees and an added Placeholder constructor 
  *)
  type patrew =
  | Placeholder of string
  | Var of string (* A placeholder that only match variables *)
  | Const of const_id * (Bitvector.t -> bool)
  (* 
    Matches a constant with a filter function.
    Assign an id to reuse it in the rewritting rules 
  *)
  | RewConst of (int -> Bitvector.t) 
  (* Only used for rewritting purposes, transform an given bitsize into an bitvector *)
  | SizeMod of sizemod_t * patrew
  | Unop of unop_t * patrew
  | Binop of binop_t * patrew * patrew
  | Triop of triop_t * patrew * patrew * patrew

  module GenC = struct
    let id : const_id ref = ref 0
    let mk_subst s = Placeholder s
    let mk_var s = Var s
    let mk_const bvf = incr id; Const (!id, bvf)
    let mk_sizemod op t = SizeMod (op,t)
    let mk_unop op t = Unop (op,t)
    let mk_binop op t1 t2 = Binop (op, t1, t2)
    let mk_triop op t1 t2 t3 = Triop (op, t1, t2, t3)
  end
  
  (* Short for Pattern Constructor, 
     used to generate the patterns part of the 
     match and replace algorithm *)
  module PC = struct
    include Treeshortcut.Make (struct 
      type t = patrew
      include GenC
    end)

    let zero = GenC.mk_const Bitvector.is_zeros
    let one = GenC.mk_const Bitvector.is_ones
    let two = GenC.mk_const (fun bv -> 
      Bitvector.(equal bv (of_int ~size:(size_of bv) 2)))
    let umaxbv = GenC.mk_const Bitvector.is_max_ubv
    let smaxbv = GenC.mk_const Bitvector.is_max_sbv
  end

  (* Short for Rewrite Constructor,
     used to generate the rewrite part of the
     match and replace algorithm *)
  module RC = struct
    include Treeshortcut.Make (struct 
      type t = patrew
      include GenC
    end)

    let mk_const_of_bv bvf = RewConst bvf
    let zero = mk_const_of_bv Bitvector.zeros
    let one = mk_const_of_bv Bitvector.ones
    let two = mk_const_of_bv (fun size -> 
      Bitvector.(of_int ~size 2))
    let umaxbv = mk_const_of_bv Bitvector.max_ubv
    let smaxbv = mk_const_of_bv Bitvector.max_sbv
  end
  
  (* We currently use an associating list for now but it may be interesting to 
     use a hast table if there is enough leafs *)
  let rec assoc_add s x = function
    | [] -> [(s, [x])]
    | ((s', l) as h) :: t -> 
      if s = s' then
        (s, x :: l) :: t
      else
        h :: (assoc_add s x t)

  let check_holders l =
    List.for_all (
      fun (_, h) ->
        match h with
        | [] -> failwith "Empty placeholder"
        | x :: t ->
          List.for_all (C.eq x) t
    ) l

  let compact_holders l =
    List.map (
      fun (s, h) -> (s, List.hd h) 
    ) l

  (* Expand the patterns containing commutative operators
     to a list pattern with all possible commutations *)
  let rec commut_pattern pat =
    match pat with
    | Binop (op, t1,t2) when Binop.is_commut op->
      let pats1 = commut_pattern t1 in
      let pats2 = commut_pattern t2 in
      List.concat_map (fun t1 ->
        List.concat_map (fun t2 -> 
          [
            GenC.mk_binop op t1 t2;
            GenC.mk_binop op t2 t1
          ]
        ) pats2
      ) pats1
    | Placeholder _ | Var _ 
    | Const _ | RewConst _ -> [pat]
    | SizeMod (op, t1) -> 
      let pats = commut_pattern t1 in
      List.map (GenC.mk_sizemod op) pats
    | Unop (op, t1) ->
      let pats = commut_pattern t1 in
      List.map (GenC.mk_unop op) pats
    | Triop (op, t1, t2, t3) ->
      let pats1 = commut_pattern t1 in
      let pats2 = commut_pattern t2 in
      let pats3 = commut_pattern t3 in
      List.concat_map (fun t1 -> 
        List.concat_map (fun t2 ->
          List.map (
            GenC.mk_triop op t1 t2
          ) pats3
        ) pats2
      ) pats1
    | Binop (op, t1, t2) -> 
      let pats1 = commut_pattern t1 in
      let pats2 = commut_pattern t2 in
      List.concat_map (fun t1 ->
        List.map (
          GenC.mk_binop op t1
        ) pats2
      ) pats1

  

  (* Matching algorithm --
     When provided with a pattern and a tree, walk the tree to match
     it against the patter.
     If successful, returns two mappings:
     - one from placeholder to tree
     - one from const_id to tree
  *)
  let match_pattern (pattern : patrew) (tree: tree) : ((string * tree) list * (const_id * tree) list) option  =
    let (>>=) = Option.bind in (* Allows shorter notation in the rest of this function *)
    let rec aux pat (t : tree) ((vholders, cholders) as holders) =
      match pat, t.node with
      | Placeholder s, _ 
      | Var s, Tree.Var _ ->
        Some (assoc_add s t vholders, cholders)
      | Const (id, f), Const c when f c.value-> 
        Some (vholders, (id, t) :: cholders)
      | SizeMod (opp,tpat), SizeMod (op, t') when SizeMod.eq op opp ->
        aux tpat t' holders
      | Unop (opp,tpat), Unop (op, t') when Unop.eq op opp ->
        aux tpat t' holders
      | Binop (opp, t1pat, t2pat), Binop (op, t1, t2) when Binop.eq op opp ->
        aux t1pat t1 holders
        >>= aux t2pat t2
      | Triop (opp, t1pat, t2pat, t3pat), Triop (op, t1, t2, t3) when Triop.eq op opp ->
        aux t1pat t1 holders
        >>= aux t2pat t2
        >>= aux t3pat t3
      | RewConst _, _ -> failwith "Tried to match a rewritter"
      | _, _ -> None
    in
    aux pattern tree ([],[])
    >>= (fun (vholders, cholders) ->
      if check_holders vholders then
        Some (compact_holders vholders, cholders)
      else
        None
    )

  (*
    Pretty-printing of variable mapping
  *)
  let pp_vholder fmt l = 
    let rec aux fmt = function
    | [] -> Format.fprintf fmt "]"
    | (s, _) :: h -> Format.fprintf fmt "(%s, _); %a" s aux h
    in
    Format.fprintf fmt "[";
    aux fmt l

  (*
    Rewrite parts of the match - rewrite algorithm --
  *)
  let rewrite_pattern (rw : patrew) bitsz (vholder, cholders) =
    let rec aux = function
    | Placeholder s
    | Var s -> (
      try List.assoc s vholder with
      | Not_found -> 
        failwith (
          Format.asprintf "Failed to found var/hole %s (vholder = %a)" s pp_vholder vholder
        )
    )
    | Const (id, _) -> List.assoc id cholders
    | RewConst bvf -> C.mk_const_of_bv (bvf bitsz)
    | SizeMod (op,tpat) ->
        C.mk_sizemod op (aux tpat)
    | Unop (op,tpat) ->
        C.mk_unop op (aux tpat)
    | Binop (op, t1pat, t2pat) ->
        C.mk_binop op (aux t1pat) (aux t2pat)
    | Triop (op, t1pat, t2pat, t3pat) ->
        C.mk_triop op (aux t1pat) (aux t2pat) (aux t3pat)
    in
    aux rw

  (* The full match and rewrite algorithm *)
  let match_and_rewrite pat rewrite t =
    match match_pattern pat t with
    | Some (vholders, cholders) -> 
      rewrite_pattern rewrite t.bitsz (vholders, cholders)
    | None -> t

  (** Syntaxique sugar to ease the writting of rewrite rules *)
  let (-->) x y = (x,y)

  let from_list (lp : (patrew * patrew) list) : tree -> tree =
    let lp = List.map (fun (x,y) -> match_and_rewrite x y) lp in
    fun t -> List.fold_left (|>) t lp

  let from_list_with_commut (lp : (patrew * patrew) list) : tree -> tree =
    let lp = List.concat_map (fun (x,y) -> List.map (fun x -> x,y) (commut_pattern x) ) lp in
    from_list lp
end

(**
  [TreeSimplifier] creates a simplier modules that performs all the registered 
  simplifications
*)
module TreeSimplifier (T : TreeConstructor) (O : Oracle.ORACLE) = struct 
  module C = UnitTreeConstructor (* This constructor uses hashconsing, allowing very fast tree equality check *)
  include C

  let setvar (var : Tree.variable) const tree =
    match tree.node with
    | Var v when String.equal var.name v.name -> const
    | _ -> tree
  
  (* Apply a transformation to all the childs of a tree *)
  let apply_to_subtree f t =
    match t.node with
    | Var _ 
    | Const _ -> t
    | SizeMod (op, t1) ->
      mk_sizemod op (f t1)
    | Unop (op, t1) ->
      mk_unop op (f t1)
    | Binop (op, t1, t2) ->
      mk_binop op (f t1) (f t2)
    | Triop (op, t1, t2, t3) ->
      mk_triop op (f t1) (f t2) (f t3)

  (* Bottom up walk in the provided tree, applying the provided transformation along the way *)
  let bottom_up_apply opt tree =
    let rec aux tree =
      opt (apply_to_subtree aux tree)
    in
    aux tree

  let sum (x : Tree.constant) (y : Tree.constant) = Tree.const_of_bitv (Binop.apply Add x.value y.value)

  let sub (x : Tree.constant) (y : Tree.constant) = Tree.const_of_bitv (Binop.apply Sub x.value y.value)

  (* Reduces involutive functions : f (f x) = x *)
  let involution t =
    match t.node with
    | Unop (ty1, { node = Unop (ty2, t'); _ })
      when Unop.eq ty1 ty2 && Unop.is_involutif ty1 -> t'
    | _ -> t

  (* Reduces nilpotent functions : f (x,x) = 0 *)
  let nilpotence t =
    match t.node with
    | Binop (ty, t1, t2)
      when Binop.is_nilpotent ty && C.eq t1 t2 ->
        mk_const_of_bv (Bitvector.zeros t.bitsz)
    | _ -> t
  
  (* Reduces idempotent functions : f(x,x) = x *)
  let idempotence t =
    match t.node with
    | Binop (ty, t1, t2)
      when Binop.is_idempotent ty && C.eq t1 t2 -> t1
    | _ -> t

  (* Reduces functions with neutral element e : f (x, e) = x or f(e, x) = x *)
  let neutral_element t =
    match t.node with
    | Binop (ty, t', { node = Const c; _ }) 
      when Binop.is_neutral_element ty ~dir:(Right ()) c.value -> t'
    | Binop (ty, { node = Const c; _ }, t')
        when Binop.is_neutral_element ty ~dir:(Left ()) c.value -> t'
    | _ -> t

  (* Reduces functions with absorbing element c : f (x, c) = c or f(c, x) = c *)
  let absorbing_element t =
    match t.node with
    | Binop (ty, _, ({ node = Const c; _ } as t')) 
      when Binop.is_absorbing_element ty ~dir:(Right ()) c.value -> t'
    | Binop (ty, ({ node = Const c; _ } as t'), _)
        when Binop.is_absorbing_element ty ~dir:(Left ()) c.value -> t'
    | _ -> t
  
  (******** Associative-Commutative Operators Handlers *******)

  (* Collect all the use of the provided operator at the top of the tree  *)
  let ac_tree_collector op t =
    let rec aux acc t = 
      match t.node with
      | Binop (op', t1, t2) when Binop.eq op op' ->
         aux (aux acc t1) t2
      | _ -> t :: acc
    in
    assert (Binop.is_assoc op && Binop.is_commut op);
    aux [] t  

  (* Regroup redundant subtrees (after sorting it first) 
     E.g. [t1,t1,t2,t3] -> [(2,t1), (1,t2), (1,t3)] *)
  let regroup l =
    let l = List.sort C.compare l in
    let rec aux acc l =
      match acc, l with
      | _, [] -> acc
      | [], h :: t -> aux [(1,h)] t
      | (n, t1) :: acc', h :: t ->
        if C.eq t1 h then
          aux ((n+1, t1) :: acc') t
        else
          aux ((1,h)::acc) t
    in
    aux [] l

  (* Create list with n times the provided element *)
  let explode (n,t) =
    List.init n (fun _ -> t)

  (* Optimize the provided tree by regrouping subtrees using the same AC-operator
     There is no cross-operator optimization
  *)
  let base_ac_opt opt t =
    let rec aux t =
      match t.node with
      | Binop (op, _, _) when 
        Binop.is_assoc op && Binop.is_commut op ->
        (* Generates an optimized list of childs *)
        let childs = 
          ac_tree_collector op t
          |> regroup
          |> opt op (* Optimize the child number with optimizations such as idempotence, nilpotence, ...  *)
          |> List.map (fun (n, t') -> n, aux t') (* Optimize the childs *)
          (* NB : Could use a specific version for various operators (e.g. if the operator is +, instead of x+x+x+x, generate 4*x) *)
          |> List.concat_map explode
        in
        (* Reduction to recreate the tree *)
        (match childs with
        | [] -> failwith "no tree found in base_ac_opt"
        | h :: l ->
          List.fold_left (mk_binop op) h l)
      | _ -> apply_to_subtree aux t
    in
    aux t

  (**** Various group optimizations for AC operators *****)
  (* Idempotent optimization *)
  let ac_idempotence op l =
    if Binop.is_idempotent op then
      List.map (fun (_,t) -> (1,t)) l
    else l
  
  (* Nilpotent optimization *)
  let ac_nilpotence op l =
    let aux (n,t) =
      if n mod 2 = 0 then (1, mk_const_of_bv (Bitvector.zeros t.bitsz))
      else (1,t)
    in
    if Binop.is_nilpotent op then 
      List.map aux l
    else l
  
  (* Constant reduction optimization *)
  let ac_constant_group_reduction op l = 
    let (consts, others) = 
      List.partition_map (function 
        | (n, { node = Const c; _ }) -> Left (n,c.value) 
        | t -> Right t
      ) l
    in
    let consts = List.concat_map explode consts in
    match consts with
    | [] -> others
    | h :: t -> 
      let c = mk_const_of_bv ( List.fold_left (Binop.apply op) h t) in
      (1,c) :: others

  let ac_opt =
    base_ac_opt (fun op l -> 
      l 
      |> (ac_idempotence op) 
      |> (ac_nilpotence op)
      |> (ac_constant_group_reduction op))

  (**** End Group optimizations for AC operators *****)
  

  (* Specialize constant propagation for associative-commutative operators *)
  let ac_constant_propagation t =
    (* Shortcut functions & values *)
    let default = t in
    let f ty (x : Tree.constant) (y : Tree.constant) = 
      Tree.const_of_bitv (Binop.apply ty x.value y.value) 
    in

    (* Core of the function *)
    match t.node with
    | Binop (ty1, t1, t2) -> (
      match t1.node, t2.node with
      | Binop (ty2, t1', t2'), Const c
      | Const c, Binop (ty2, t1', t2') 
            when Binop.eq ty1 ty2 
            && Binop.is_commut ty1 && Binop.is_assoc ty1 -> (
          match (t1'.node, t2'.node) with
          | Const c1, Const c2 -> mk_const (f ty1 c2 (f ty1 c1 c))
          | _, Const c1 -> mk_binop ty1 t1' (mk_const (f ty1 c c1))
          | Const c1, _ -> mk_binop ty1 t2' (mk_const (f ty1 c c1))
          | _ -> default
          )
      | _ -> default
    )
    | _ -> default
  
  (******** End of Associative-Commutative Operators Handlers *******)

  (* Constant propagation optimization *)
  let constant_propagation t =
    let t = ac_constant_propagation t in

    (* Shortcut functions & values *)
    let default = t in
    let all_zero t = mk_const_of_bv (Bitvector.zeros t.bitsz) in
    let all_one t = mk_const_of_bv (Bitvector.max_ubv t.bitsz) in

    match t.node with
    (* Generic local constant propagation *)
    | Triop (ty, { node = Const c1; _ }, { node = Const c2; _ }, { node = Const c3; _ } ) ->
        mk_const_of_bv (Triop.apply ty c1.value c2.value c3.value)
    | Binop (ty, { node = Const c1; _ }, { node = Const c2; _ }) ->
        mk_const_of_bv (Binop.apply ty c1.value c2.value)
    | Unop (ty, { node = Const c1; _ }) ->
        mk_const_of_bv (Unop.apply ty c1.value)
    | SizeMod (ty, { node = Const c1; _ }) ->
        mk_const_of_bv (SizeMod.apply ty c1.value)
    
    (* ITE / Im *)
    | Triop (ITE, { node = Const c; _ }, t2, t3) ->
        if Bitvector.is_zeros c.value then t3 else t2
    | Triop (Im, { node = Const c; _ }, t2, t3) ->
        if Bitvector.is_ones c.value then t2 else t3

    (* Div / SDiv *)
    | Triop (Div, _, _, ({ node = Const c; _} as t3) )
          when Bitvector.is_zeros c.value -> all_one t3
    | Triop (Div, _, t2, { node = Const c; _} )
    | Triop (SDiv, _, t2, { node = Const c; _} )
          when Bitvector.is_ones c.value ->
          t2 (* only t2 because we reduce to size bits*)
    
    (* Mod / SMod *)
    | Triop (Mod, _, _, ({ node = Const c; _} as t3) )
    | Triop (SMod, _, _, ({ node = Const c; _} as t3) )
          when Bitvector.is_ones c.value -> all_zero t3
    
    (* Add *)
    | Binop (Add , t1, t2) -> (
      match (t1.node, t2.node) with
      (* (c1 - b) + c2 = (c1 + c2) - b *)
      | Binop (Sub, t1', t2'), Const c 
      | Const c, Binop (Sub, t1', t2') -> (
        match (t1'.node, t2'.node) with
        | Const c1, Const c2 -> mk_const (sum (sub c1 c2) c)
        | _, Const c1 -> mk_binop Add t1' (mk_const (sub c c1))
        | Const c1, _ -> mk_binop Sub (mk_const (sum c c1)) t2'
        | _, _ -> default)

      | _ -> default
    )

    (* Mul *)
    | Binop (Mul, t1, t2) -> (
      match (t1.node, t2.node) with
      (* TODO : Requires to check that Minus is in the grammar *)
      (* a * -1 = -a *)
      | Const c, _ when Bitvector.is_max_ubv c.value ->
        mk_unop Minus t2
      | _, Const c when Bitvector.is_max_ubv c.value ->
        mk_unop Minus t1
      | _ -> default
    )

    (* Sub *)
    | Binop (Sub, t1, t2) -> (
        match (t1.node, t2.node) with
        (* TODO : Requires to check that Minus is in the grammar *)
        (* 0 - a = -a *)
        | Const c, _ when Bitvector.is_zeros c.value -> mk_unop Minus t2
        
        (* TODO : Requires to check that Add is in the grammar *)
        (* (c1 - b) - c2 = (c1 - c2) - b *)
        | Binop (Sub, t1', t2'), Const c -> (
            match (t1'.node, t2'.node) with
            | Const c1, Const c2 -> mk_const (sub (sub c1 c2) c)
            | _, Const c1 -> mk_binop Sub t1' (mk_const (sum c c1))
            | Const c1, _ -> mk_binop Sub (mk_const (sub c1 c)) t2'
            | _, _ -> default)
        | Const c, Binop (Sub, t1', t2') -> (
            match (t1'.node, t2'.node) with
            | Const c1, Const c2 -> mk_const (sub c (sub c1 c2))
            | _, Const c1 -> mk_binop Sub (mk_const (sum c c1)) t1'
            | Const c1, _ -> mk_binop Add (mk_const (sub c c1)) t2'
            | _, _ -> default)
        | Const c, Binop (Add, t1', t2') -> (
            match (t1'.node, t2'.node) with
            | Const c1, Const c2 -> mk_const (sub c (sum c1 c2))
            | Const c1, _ -> mk_binop Sub (mk_const (sub c c1)) t2'
            | _, Const c2 -> mk_binop Sub (mk_const (sub c c2)) t1'
            | _, _ -> default)
        | Binop (Add, t1', t2'), Const c -> (
            match (t1'.node, t2'.node) with
            | Const c1, Const c2 -> mk_const (sub (sum c1 c2) c)
            | Const c1, _ -> mk_binop Sub t2' (mk_const (sub c c1))
            | _, Const c2 -> mk_binop Sub t1' (mk_const (sub c c2))
            | _, _ -> default)

        | _ -> default
    )

    (* And *)
    | Binop (And, t1, t2) -> (
        match (t1.node, t2.node) with
        (* 1 & (a * b) = 0 if a % 2 = 0 *)
        | Const c, Binop (Mul, t21', t22') 
        | Binop (Mul, t21', t22'), Const c
              when Bitvector.is_ones c.value -> (
            match (t21'.node, t22'.node) with
            | Const c, _ when not (Bitvector.get_bit c.value 0) ->
              (* if const is even *)
              all_zero t21'
            | _, Const c when not (Bitvector.get_bit c.value 0) ->
              (* if const is even *)
              all_zero t22'
            | _, _ -> default
            )
        
        | _ -> default
    )

    (* Xor *)
    | Binop (Xor, t1, t2) -> (
        match (t1.node, t2.node) with
        (* a ^ -1 = ~a *)
        | _, Const c when Bitvector.is_max_ubv c.value -> mk_unop Not t1
        | Const c, _ when Bitvector.is_max_ubv c.value -> mk_unop Not t2
        | _ -> default
    )

    (* Minus *)
    (* - (a + 1) = ~a *)
    | Unop (Minus, { node = Binop(Add, t1', t2'); _}) -> (
        match (t1'.node, t2'.node) with
        | _, Const c when Bitvector.is_ones c.value ->
            mk_unop Not t1'
        | Const c, _ when Bitvector.is_ones c.value ->
            mk_unop Not t2'
        | _, _ -> default)

    | _ -> default    

  (* Specialized optimizations (written in pattern / rewrite format) 
     for each operator *)
  let pat_specialize_opt =
    let open Pattern in
    let t1 = GenC.mk_subst "t1" in
    let t2 = GenC.mk_subst "t2" in
    let _t3 = GenC.mk_subst "t3" in
    let x = GenC.mk_var "x" in
    let _y = GenC.mk_var "y" in

    let add_pat = [
      (* (a ^ b) + (a & b) = a | b (TODO : Check grammar for the existence of OR)*)
      PC.( (t1 ^ t2) + (t1 && t2) ) --> RC.(t1 || t2);

      (* (a - b) + b = a *)
      PC.( (t1 - t2) + t2 ) --> t1;

      (* (- a) + b = b - a *)
      PC.( - t1 + t2 ) --> RC.( t2 - t1 );

      (* ~a + a = -1 *)
      PC.( (~. t1) + t1 ) --> RC.umaxbv;

      (* ~a + 1 = -a *)
      PC.( (~. t1) + one ) --> RC.( - t1);
         
      (* ~a + (a + b) = b - 1 *)
      PC.( (~. t1) + (t1 + t2) ) --> RC.( t2 - one );

      (* a + a = 2*a *)
      PC.( x + x ) --> RC.( two * x );
    ] 
    and mul_pat = [
      (* -a * -b = a * b *)
      PC.( (-t1) * (-t2)) --> RC.(t1 * t2);

      (* -a * b = - (a * b) *)
      PC.( (-t1) * t2) --> RC.(- (t1 * t2));
    ]
    and sub_pat = [
      (* -a - 1 = ~a *)
      PC.( (- t1) - one ) --> RC.( ~. t1 );
      
      (* a - (-b) = a + b *)
      PC.( t1 - ( -t2 ) ) --> RC.( t1 + t2 );
      
      (* -a - b = - (a + b) *)
      PC.( (- t1) - t2 ) --> RC.( - ( t1 + t2 ) );
    ]
    and and_pat = [
      (* (a ^ b) & (a & b) = 0 *)
      PC.( (t1 ^ t2) && ( t1 && t2 ) ) --> RC.( zero );

      (* (a ^ b) & a = a & ~b *)
      PC.( (t1 ^ t2) && t1 ) --> RC.( t1 && (~. t2) );

      (* NB : These three patterns could be handled by
        some pattern detection in AC operator sub-graph *)
      (* a & ~a  = 0 *)
      PC.( t1 && ( ~. t1 ) ) --> RC.( zero );

      (* (a & b) & ~a = 0 *)
      PC.( (t1 && t2) && ( ~. t1 ) ) --> RC.( zero );

      (* a & (~a & b) = 0 *)
      PC.( t1 && ( (~. t1) && t2) ) --> RC.( zero );
    ]
    and or_pat = [
      (* (a ^ b) | a = a | b *)
      PC.( (t1 ^ t2) || t1 ) --> RC.( t1 || t2 );

      (* a | ~a = -1 *)
      PC.( t1 || (~. t1) ) --> RC.( umaxbv );

      (* (a & b) | a = a *)
      PC.( ( t1 && t2 ) || t1 ) --> t1;

      (* (a | b) | ~a = -1 *)
      PC.( ( t1 || t2 ) || ( ~. t1 ) ) --> RC.( umaxbv );
    ]
    and xor_pat = [
      (* a ^ ~a = -1 *)
      PC.( t1 ^ ( ~. t1 ) ) --> RC.( umaxbv );

      (* (-(1 | a)) ^ (1 | a) = -2 *)
      PC.( (- (one || t1)) ^ (one || t1) ) --> RC.( - two );
    ]
    in
    from_list_with_commut (
      add_pat @ mul_pat @ sub_pat
      @ and_pat @ or_pat @ xor_pat
    )


  (* Specialized optimization *)
  let specialize_local_opt (t : C.data t) : C.data t =
    (* Shortcut functions & values *)
    let default = t in
    let all_zero t = mk_const_of_bv (Bitvector.zeros t.bitsz) in
    let one t = mk_const_of_bv (Bitvector.ones t.bitsz) in

    match t.node with
    (* ITE / Im optimizations *)
    | Triop (ITE, ({ node = Var v; _ } as t1), t2, t3) ->
        let t3 = bottom_up_apply (setvar v (all_zero t3)) t3 in
        mk_triop ITE t1 t2 t3
    | Triop (Im, ({ node = Var v; _ } as t1), t2, t3) ->
        let t2 = bottom_up_apply (setvar v (one t2)) t2 in
        mk_triop Im t1 t2 t3
    (* Mod optimizations *)
    (* TODO : This is not entirely true (fails for t3 == 0) *)
    | Triop (Mod, t1, t2, t3) 
          when C.eq t1 t2 && C.eq t1 t3 -> all_zero t1
    | _ -> default

  (* Repeat optimisations until there is no more changes *)
  let rec opt_saturation opt tree =
    let ntree = bottom_up_apply opt tree in
    let ntree = ac_opt ntree in
    if C.eq tree ntree then tree
    else
      opt_saturation opt ntree

  (* Core function assembling all the previous ones *)
  let simplify tree =
    let t = C.transfer tree in
    let aux t =
      t
      |> involution
      |> neutral_element
      |> constant_propagation
      |> idempotence
      |> nilpotence
      |> absorbing_element
      |> specialize_local_opt
      |> pat_specialize_opt
    in 
    opt_saturation aux t
    |> T.transfer
end