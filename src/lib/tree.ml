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

open Utility

let select omega =
    omega.(Random.int (Array.length omega))

module type XOP = sig
    type t
    val rand : unit -> t   
    val cardinal : unit -> int
    val to_string : t -> string
    val to_smtlib : t -> string
end

type unop_t = Minus | Not | Extend of int | Sextend of int | Reduce of int | Byteswap | Ehad | Arba | Shesh | Smol

module type UNOP = sig
    include XOP with type t := unop_t
    val apply : unop_t -> Bitvector.t -> Bitvector.t
    val is_sz_modifier : unop_t -> bool

    val to_sygus : unop_t -> string -> int -> string
end

module Mk_Unop (Oracle : Oracle.ORACLE) : UNOP = struct
    let number = 7
    let of_string = function
    | "not" -> Some Not
    | "neg" -> Some Minus
    | "bswap" -> Some Byteswap
    | "ehad" -> Some Ehad
    | "arba" -> Some Arba
    | "shesh" -> Some Shesh
    | "smol" -> Some Smol
    | _ -> None

    let of_int = function
    | 0 -> Minus
    | 1 -> Not
    | 2 -> Byteswap
    | 3 -> Byteswap
    | 4 -> Ehad
    | 5 -> Arba
    | 6 -> Shesh
    | 7 -> Smol
    | _ -> assert false

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let unops = Array.of_list (List.filter_map of_string ops) in
            if Array.length unops = 0 then [| of_int 0 |] else unops

    let rand () =  select omega
    let cardinal () = Array.length omega
    let is_sz_modifier = function
    | Extend _ | Sextend _ | Reduce _ -> true
    | _ -> false

    let apply op x = match op with
    | Minus -> Bitvector.neg x
    | Not -> Bitvector.lognot x
    | Extend sz -> Bitvector.extend x sz
    | Sextend sz -> Bitvector.extend_signed x sz
    | Reduce sz -> Bitvector.reduce x sz
    | Byteswap -> Bitvector.byteswap x
    | Ehad -> Bitvector.shift_right x 1
    | Arba -> Bitvector.shift_right x 4
    | Shesh -> Bitvector.shift_right x 16
    | Smol -> Bitvector.shift_left x 1

    let to_string = function
    | Minus -> "-"
    | Not -> "not"
    | Extend sz -> Printf.sprintf "%dext" sz
    | Sextend sz -> Printf.sprintf "%dsext" sz
    | Reduce sz -> Printf.sprintf "%dred" sz
    | Byteswap -> "bswap"
    | Ehad -> "ehad"
    | Arba -> "arba"
    | Shesh -> "shesh"
    | Smol -> "smol"

    let to_smtlib = function
    | Minus -> "bvneg"
    | Not -> "bvnot"
    | Extend sz -> Printf.sprintf "(_ zero_extend %d)" sz
    | Sextend sz -> Printf.sprintf "(_ sign_extend %d)" sz
    | Reduce sz -> Printf.sprintf "(_ extract %d 0)" (sz-1)
    | Byteswap -> "bswap"
    | Ehad -> "ehad"
    | Arba -> "arba"
    | Shesh -> "shesh"
    | Smol -> "smol"

    let to_sygus op term1 sz1 = match op with
    | Minus -> Format.sprintf "(bvneg %s)" term1
    | Not -> Format.sprintf "(bvnot %s)" term1
    | Extend sz -> Printf.sprintf "((_ zero_extend %d) %s)" (sz - sz1) term1
    | Sextend sz -> Printf.sprintf "((_ sign_extend %d) %s)" (sz - sz1) term1
    | Reduce sz -> Printf.sprintf "(_ extract %d 0)" (sz-1)
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

type binop_t = Add | Sub | Mul | And | Or | Xor | RShiftu | LShift | RShifts | RotateRight

module type BINOP = sig
    include XOP with type t := binop_t
    val apply : binop_t -> Bitvector.t -> Bitvector.t -> Bitvector.t
    val to_sygus : binop_t -> string -> int -> string -> int -> string
end

module Mk_Binop(Oracle : Oracle.ORACLE) : BINOP = struct
    let number = 10 (* number of operators*)

    let of_string = function
    | "add" -> Some Add
    | "sub" -> Some Sub
    | "imul" -> Some Mul
    | "and" -> Some And
    | "or" -> Some Or
    | "xor" -> Some Xor
    | "rshiftu" -> Some RShiftu
    | "lshift" -> Some LShift
    | "rshifts" -> Some RShifts
    | "ror" -> Some RotateRight
    | _ -> None

    let of_int = function
    | 0 -> Add
    | 1 -> Sub
    | 2 -> Mul
    | 3 -> And
    | 4 -> Or
    | 5 -> Xor
    | 6 -> RShiftu
    | 7 -> LShift
    | 8 -> RShifts
    | 9 -> RotateRight
    | _ -> assert false

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let binops = Array.of_list (List.filter_map of_string ops) in
            if Array.length binops = 0 then [| of_int 0 |] else binops

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
    | RotateRight -> "rotate_right"

    let to_sygus op term1 _sz1 term2 _sz2 = match op with
    | Add -> Format.sprintf "(bvadd %s %s)" term1 term2
    | Sub -> Format.sprintf "(bvsub %s %s)" term1 term2
    | Mul -> Format.sprintf "(bvmul %s %s)" term1 term2
    | And -> Format.sprintf "(bvand %s %s)" term1 term2
    | Or  -> Format.sprintf "(bvor %s %s)" term1 term2
    | Xor -> Format.sprintf "(bvxor %s %s)" term1 term2
    | RShiftu -> Format.sprintf "(bvlshr %s %s)" term1 term2
    | LShift -> Format.sprintf "(bvshl %s %s)" term1 term2
    | RShifts -> Format.sprintf "(bvashr %s %s)" term1 term2
    | RotateRight -> Format.sprintf "((_ rotate_right %s) %s)" term1 term2

    let rand () = select omega
    let cardinal () = Array.length omega

    let unsigned_int x size = (x + (1 lsl size)) mod (1 lsl size) 

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
            let offset = unsigned_int (Bitvector.to_int y) (Bitvector.size_of y) in
            Bitvector.shift_right x offset 
        | LShift -> 
            let offset = unsigned_int (Bitvector.to_int y) (Bitvector.size_of y) in
            Bitvector.shift_left x offset 
        | RShifts -> 
            let offset = Bitvector.to_int y in 
            if offset < 0 && (Bitvector.sgt x (Bitvector.zeros size)) then 
                Bitvector.zeros size
            else if offset < 0 && (Bitvector.sle x (Bitvector.zeros size)) then 
                Bitvector.ones size
            else 
                Bitvector.shift_right_signed x offset
        | RotateRight ->
            let offset = Bitvector.to_int y in
            Bitvector.rotate_right x offset
end

type triop_t = Div | SDiv | Mod | SMod | ITE | Im

module type TRIOP = sig
    include XOP with type t := triop_t
    val apply : triop_t -> Bitvector.t -> Bitvector.t -> Bitvector.t -> Bitvector.t
    val to_sygus : triop_t -> string -> int -> string -> int -> string -> int -> string
end

module Mk_Triop(Oracle : Oracle.ORACLE) : TRIOP = struct
    let number = 6
    let of_string = function
    | "div" -> Some Div
    | "sdiv" -> Some SDiv
    | "umod" -> Some Mod
    | "smod" -> Some SMod
    | "ite" -> Some ITE
    | "im" -> Some Im
    | _ -> None
    let of_int = function
    | 0 -> Div
    | 1 -> SDiv
    | 2 -> Mod
    | 3 -> SMod
    | 4 -> ITE
    | 5 -> Im
    | _ -> assert false 

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let triops = Array.of_list (List.filter_map of_string ops) in
            if Array.length triops = 0 then [| of_int 0 |] else triops

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
    | Im -> "im"

    let to_sygus op term1 sz1 term2 sz2 term3 sz3 = 
    let catsize = sz1 + sz2 in
    let size = sz3 in
    match op with
    | Div -> Format.sprintf "((_ extract %d 0) (bvudiv (concat %s %s) ((_ zero_extend %d) %s)))" (size-1) term1 term2 (catsize - size) term3
    | SDiv -> Format.sprintf "((_ extract %d 0) (bvsdiv (concat %s %s) ((_ sign_extend %d) %s)))" (size-1) term1 term2 (catsize - size) term3
    | Mod -> Format.sprintf "((_ extract %d 0) (bvurem (concat %s %s) ((_ zero_extend %d) %s)))" (size-1) term1 term2 (catsize - size) term3
    | SMod -> Format.sprintf "((_ extract %d 0) (bvsrem (concat %s %s) ((_ sign_extend %d) %s)))" (size-1) term1 term2 (catsize - size) term3
    | ITE -> Format.sprintf "(ite (distinct  %s (_ bv0 %d)) %s %s)" term1 sz1 term2 term3
    | Im -> Format.sprintf "(ite (= %s %s) %s %s)" term1 (cst_to_smtlib 1 sz1) term2 term3

    let rand () = select omega

    let cardinal () = Array.length omega

    let apply op x y z = 
        let _ = if (Bitvector.size_of x) != (Bitvector.size_of y) || (Bitvector.size_of y) != (Bitvector.size_of z) then failwith "Triop.apply: different sizes" in
        let catsize = (Bitvector.size_of x) + (Bitvector.size_of y) in                                                                                                                                             
        let size = (Bitvector.size_of z) in
        match op with
        | Div ->
            if Bitvector.is_zeros z then
                Bitvector.max_ubv size
            else
                Bitvector.extract (Bitvector.udiv (Bitvector.concat [x; y]) (Bitvector.extend z catsize)) { lo=0; hi=size-1}
        | SDiv -> 
            let cat = Bitvector.concat [x; y] in
            if Bitvector.is_zeros z then
                if Bitvector.sge cat (Bitvector.zeros catsize) then
                    Bitvector.max_ubv size
                else
                    Bitvector.ones size
            else
                Bitvector.extract (Bitvector.sdiv cat (Bitvector.extend_signed z catsize)) {lo=0; hi=(size-1)}
        | Mod ->
            let cat = Bitvector.concat [x; y] in
            let divisor = Bitvector.extend z catsize in
            if Bitvector.is_zeros divisor then
                if Bitvector.equal cat divisor then
                    Bitvector.zeros size
                else
                    y
            else
                Bitvector.extract (Bitvector.umod cat divisor) {lo=0; hi=(size-1)}

        | SMod -> 
            let cat = Bitvector.concat [x; y] in
            let divisor = Bitvector.extend_signed z catsize in
            if Bitvector.is_zeros divisor then
                y
            else
                Bitvector.extract (Bitvector.smod cat divisor) {lo=0; hi=(size-1)}

        | ITE -> 
                if Bitvector.is_zeros x then
                    z
                else
                    y
        | Im -> if (Bitvector.equal x (Bitvector.ones (Bitvector.size_of x))) then y else z
end

type op_t = OP_Var of Oracle.variable | OP_Const of Oracle.constant | OP_Unop of unop_t | OP_Binop of binop_t | OP_Triop of triop_t

module type MUTATION_BASE = sig
    val spe_ops: op_t array
end

module type MUTATIONS = sig
    val all_mut : unit -> op_t array
    val unop_mut : unit -> op_t array
    val binop_mut : unit -> op_t array
    val triop_mut : unit -> op_t array
    val unop_sygus : op_t -> string -> int -> string
    val binop_sygus : op_t -> string -> int -> string -> int -> string
    val triop_sygus : op_t -> string -> int -> string -> int -> string -> int -> string
end

module Mk_Mutations(M: MUTATION_BASE) (O : Oracle.ORACLE) : MUTATIONS = struct 
    module Unop = Mk_Unop (O) 
    module Binop = Mk_Binop (O) 
    module Triop = Mk_Triop (O) 

    let all_mut () = 
        let consts = Array.map (fun x -> OP_Const x) (O.consts ()) in
        let vars = Array.map (fun x -> OP_Var x) (O.vars ()) in
        Array.concat [ consts ; vars; M.spe_ops ]

    let unop_mut () = 
        Array.to_list (all_mut ()) 
        |> List.filter (function
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false)
        |> Array.of_list

    let binop_mut () = 
        Array.to_list (all_mut ()) 
        |> List.filter (function
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false)
        |> Array.of_list

    let triop_mut () = 
        Array.to_list (all_mut ()) 
        |> List.filter (function
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false)
        |> Array.of_list
    
    let unop_sygus op nonterm1 sz1 = match op with
    | OP_Const(c) -> 
        let z = Bitvector.signed_of c.value in
        let bv = Bitvector.create z sz1 in
        bv_to_smtlib bv
    | OP_Var(v) -> if v.sz == sz1 then v.name else ""
    | OP_Unop(unop) -> Unop.to_sygus unop nonterm1 sz1
    | _ -> failwith "not a unop"  

    let binop_sygus op nonterm1 sz1 nonterm2 sz2 = match op with
    | OP_Const(_) -> ""
    | OP_Var(_) -> ""
    | OP_Binop(binop) -> Binop.to_sygus binop nonterm1 sz1 nonterm2 sz2
    | _ -> failwith "not a binop" 
    
    let triop_sygus op nonterm1 sz1 nonterm2 sz2 nonterm3 sz3 = match op with
    | OP_Const(_) -> ""
    | OP_Var(_) -> ""
    | OP_Triop(triop) -> Triop.to_sygus triop nonterm1 sz1 nonterm2 sz2 nonterm3 sz3
    | _ -> failwith "not a triop" 
end

module Mk_Mutations_Five(O : Oracle.ORACLE) : MUTATIONS = 
    Mk_Mutations(struct 
        let spe_ops = [|
            (* OP_Const(O.random_const ());
            OP_Var(O.random_var ()); *)

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
            (* OP_Const(O.random_const ());
            OP_Var(O.random_var ()); *)

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
            (* OP_Const(O.random_const ());
                OP_Var(O.random_var ()); *)

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
            (* OP_Const(O.random_const ());
                OP_Var(O.random_var ()); *)

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
            (* OP_Const(O.random_const ());
                OP_Var(O.random_var ()); *)

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
            (* OP_Const(O.random_const ());
                OP_Var(O.random_var ()); *)

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

module Mk_Mutations_Full(O : Oracle.ORACLE) : MUTATIONS = 
    Mk_Mutations(struct 
        let spe_ops = [|
            (* OP_Const(O.random_const ());
                OP_Var(O.random_var ()); *)

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

let mutations_of_str (module O: Oracle.ORACLE) str = match str with
| "5" -> (module Mk_Mutations_Five (O) : MUTATIONS)
| "mba" -> (module Mk_Mutations_Mba (O) : MUTATIONS)
| "expr" -> (module Mk_Mutations_Expr (O) : MUTATIONS)
| "expr_ite" -> (module Mk_Mutations_Expr_ITE (O) : MUTATIONS)
| "mba_shift" -> (module Mk_Mutations_Mba_Shift (O) : MUTATIONS)
| "mba_ite" -> (module Mk_Mutations_Mba_ITE (O) : MUTATIONS)
| "full" -> (module Mk_Mutations_Full (O) : MUTATIONS)
| "sygus" -> (module Mk_Mutations_Sygus (O) : MUTATIONS)
| _ -> assert false

(* Create the set of mutations from a user given list of operator's string representation *)
let mutations_of_operators (module O: Oracle.ORACLE) arr = 
    let muts = Array.map (fun s -> 
        match s with
        | "neg" -> OP_Unop(Minus)
        | "not" -> OP_Unop(Not)
        | "+" -> OP_Binop(Add)
        | "-" -> OP_Binop(Sub)
        | "*" -> OP_Binop(Mul)
        | "&" -> OP_Binop(And)
        | "|" -> OP_Binop(Or)
        | "^" -> OP_Binop(Xor)
        | ">>" -> OP_Binop(RShiftu)
        | ">>s" -> OP_Binop(RShifts)
        | "<<" -> OP_Binop(LShift)
        | "/" -> OP_Triop(Div)
        | "/s" -> OP_Triop(SDiv)
        | "%" -> OP_Triop(Mod)
        | "%s" -> OP_Triop(SMod)
        | "ite" -> OP_Triop(ITE)
        | "bswap" -> OP_Unop(Byteswap)
        | "ehad" -> OP_Unop(Ehad)
        | "arba" -> OP_Unop(Arba)
        | "shesh" -> OP_Unop(Shesh)
        | "smol" -> OP_Unop(Smol)
        | "im" -> OP_Triop(Im)
        | _ -> failwith "Unknow operator") arr
    in
    (module (Mk_Mutations (struct 
        let spe_ops = Array.append [| OP_Const(O.random_const ()); OP_Var(O.random_var ()) |] muts
    end) (O)) : MUTATIONS)

type node =
    | Var  of Oracle.variable
    | Const of Oracle.constant
    | Unop  of unop_t * t
    | Binop of binop_t * t * t
    | Triop of triop_t * t * t * t
and t = {
    node : node ;
    (* number of nodes in this subtree, size modifiers do not count *)
    sz : int ;
    (* bitsize of the values calculated by this tree *)
    bitsz : int ;
    mutable last_mutation_indice : int option ;
    all_choices : op_t array;
    mutable left_choices : op_t array;
    vals : Bitvector.t array }

module type MUTATOR = sig
    val singleton : unit -> t
    val mutate : int -> t -> int option -> t
    val mutate_mh : int -> t -> int option -> t * float
    val cut : t -> t
    val eval : t -> Bitvector.t array
    val to_string : t -> string
    val to_smtlib : t -> string
    val simplify: t -> t
    val get_expr_size : t -> int
    val get_mutations : unit -> (module MUTATIONS)
    val mk_var : Oracle.variable -> t
    val mk_const_of_bv : Bitvector.t -> t
    val mk_unop : unop_t -> t -> t
    val mk_binop : binop_t -> t -> t -> t
    val mk_triop : triop_t -> t -> t -> t -> t
end

module Mk_Mutator(O : Oracle.ORACLE) (M : MUTATIONS) : MUTATOR = struct
    module Unop = Mk_Unop(O)
    module Binop = Mk_Binop(O)
    module Triop = Mk_Triop(O)
    include M

    exception NOCHOICE

    let mk_var var = {
        node = Var var ;
        sz = 1 ;
        bitsz = var.sz ;
        vals = O.var_values var ;
        last_mutation_indice = None;
        all_choices = all_mut (); 
        left_choices = all_mut (); 
    }

    let mk_const c = {
        node = Const c ;
        sz = 1 ;
        bitsz = Bitvector.size_of c.value ;
        vals = O.const_values c;
        last_mutation_indice = None;
        all_choices = all_mut (); 
        left_choices = all_mut (); 
    }

    let mk_const_of_bv bv = 
        mk_const (O.const_of_bitv bv)
    
    let mk_unop op t =
        let (sz, bitsz) =
            match op with
            | Sextend x | Extend x | Reduce x -> (t.sz, x)
            | _ -> (t.sz + 1, t.bitsz)
        in
        {
            node = Unop (op, t) ;
            sz ;
            bitsz ;
            vals = Array.map (fun x -> Unop.apply op x) t.vals;
            last_mutation_indice = None;
            all_choices = unop_mut (); 
            left_choices = unop_mut ();
        }

    let mk_binop op t1 t2 = {
        node = Binop (op, t1, t2) ;
        sz = 1 + t1.sz + t2.sz ;
        bitsz = t1.bitsz ;
        vals = Array.map2 (fun x y -> Binop.apply op x y) t1.vals t2.vals;
        last_mutation_indice = None;
        all_choices = binop_mut (); 
        left_choices = binop_mut (); 
    }

    let mk_triop op t1 t2 t3 = {
        node = Triop (op, t1, t2, t3) ;
        sz = 1 + t1.sz + t2.sz + t3.sz ; (* TODO Check *)
        bitsz = t1.bitsz ;
        vals = Utility.map3 (fun x y z -> Triop.apply op x y z) t1.vals t2.vals t3.vals;
        last_mutation_indice = None;
        all_choices = triop_mut (); 
        left_choices = triop_mut ();
    }

    let mk_terminal bitsz leaf =
        if leaf.bitsz = bitsz then leaf
        else if leaf.bitsz < bitsz && Random.bool () then
            mk_unop (Extend bitsz) leaf
        else if leaf.bitsz < bitsz then
            mk_unop (Sextend bitsz) leaf
        else mk_unop (Reduce bitsz) leaf

    let get_random_left_choice t indice = 
        let choices = match t.last_mutation_indice, indice with
        | Some li, Some i -> 
            if  li < i then
                let _ = t.last_mutation_indice <- indice in
                t.all_choices
            else
                t.left_choices
        | None, Some _ -> 
                let _ = t.last_mutation_indice <- indice in
                t.all_choices
        | Some _, None -> assert false
        | None , None -> 
                t.all_choices
        in
        let size = Array.length choices in
        if size = 0 then
            None
        else
            let value = Random.int size in
            let res = choices.(value) in
            let _ = match indice with
            | Some _ -> t.left_choices <- Array.init (size-1) (fun i -> if i < value then choices.(i) else choices.(i+1))
            | None -> ()
            in
            Some res
        
    let random_terminal (bitsz : int) : t =
        let nvars = O.nvars () in
        let nconsts = O.nconsts () in
        let choice = Random.int (nvars + nconsts) in
        if choice < nvars then
            mk_terminal bitsz (mk_var (O.random_var ()))
        else mk_terminal bitsz (mk_const (O.random_const ()))

    let terminal_from_const c (bitsz : int) = 
        mk_terminal bitsz (mk_const c)

    let terminal_from_var v (bitsz : int) = 
        mk_terminal bitsz (mk_var v)

    let random_unop ?op bitsz = match op with
    | Some operator -> mk_unop operator (random_terminal bitsz)
    | None -> mk_unop (Unop.rand ()) (random_terminal bitsz)

    let random_binop ?op bitsz = match op with
    | Some operator -> 
        mk_binop operator
            (random_terminal bitsz)
            (random_terminal bitsz)
    | None ->
        mk_binop (Binop.rand ())
            (random_terminal bitsz)
            (random_terminal bitsz)

    let random_triop ?op bitsz = match op with
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

    let is_terminal t =
        let is_leaf t = match t.node with
        | Var _ | Const _ -> true
        | _ -> false
        in
        is_leaf t || match t.node with
            | Unop (Extend _, t') | Unop (Sextend _, t')
            | Unop (Reduce _, t') -> is_leaf t'
            | _ -> false

    let rec tree_equal t1 t2 = match t1.node, t2.node with
    | Const c1, Const c2 when c1 = c2 -> true
    | Var v1, Var v2 when v1 = v2 -> true
    | Unop(op1, t1'), Unop(op2, t2') when op1 = op2 -> 
        tree_equal t1' t2'
    | Binop(op1, t11', t12'), Binop(op2, t21', t22') when op1 = op2 -> 
        begin match op1 with
        | Sub | LShift | RShifts | RShiftu | RotateRight ->
            (tree_equal t11' t21' && tree_equal t12' t22')
        | _ ->
            (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') 
        end
    | Triop(op1, t11', t12', t13'), Triop(op2, t21', t22', t23') when op1 = op2 -> 
        begin match op1 with
        | Div | SDiv | Mod | SMod | ITE | Im ->
            (tree_equal t11' t21' && tree_equal t12' t22' && tree_equal t13' t23')
        end
    | _, _ -> false

    let mutate height_lim t indice =
        let rec aux d id t indice = match t.node with
        | Var _ | Const _ -> mutate_leaf d t
        | Unop (op, t') ->
            if is_terminal t then mutate_leaf d t
            else if Unop.is_sz_modifier op then aux d id t' indice
            else if id = t.sz then mutate_unop  t
            else 
                let tree = (aux  (d + 1) id t' indice) in
                mk_unop op tree
        | Binop (op, t1, t2) ->
            let rank = 1 + t1.sz in
            if id = rank then mutate_binop  t
            else if id < rank then 
                let tree = (aux  (d + 1) id t1 indice) in
                mk_binop op tree t2
            else 
                let tree = (aux  (d + 1) (id - rank) t2 indice) in
                mk_binop op t1 tree
        | Triop (op, t1, t2, t3) ->
            let rank = 1 + t1.sz in
            if id = rank then mutate_triop  t
            else if id < rank then 
                let tree = (aux  (d + 1) id t1 indice) in
                mk_triop op  tree t2 t3
            else if id > rank + t2.sz then 
                let tree = (aux  (d + 1) (id - (rank + t2.sz)) t3 indice) in
                mk_triop op t1 t2 tree
            else 
                let tree = (aux  (d + 1) (id - rank) t2 indice) in
                mk_triop op t1 tree t3
        and mutate_leaf d t =
            if d = height_lim then 
                random_terminal t.bitsz 
            else
                let choice = match get_random_left_choice t indice with 
                | Some v -> v
                | None -> raise NOCHOICE
                in
                begin match choice with
                | OP_Const(c) -> terminal_from_const c t.bitsz
                | OP_Var(v) -> terminal_from_var v t.bitsz
                | OP_Unop(op) -> random_unop ~op:op t.bitsz
                | OP_Binop(op) -> random_binop ~op:op t.bitsz
                | OP_Triop(op) -> random_triop ~op:op t.bitsz
                end

                (* if 0 <= choice && choice < nconsts then 
                    let op = (O.consts ()).(choice) in
                    terminal_from_const op t.bitsz
                else if nconsts <= choice && choice < nterms  then 
                    let op = (O.vars ()).(choice - nconsts) in
                    terminal_from_var op t.bitsz
                else if nterms <= choice && choice < nterms + Unop.number then
                    let op = Unop.of_int (choice - nterms) in 
                    random_unop ~op:op t.bitsz
                else if nterms + Unop.number <= choice && choice < nterms + Unop.number + Binop.number then 
                    let op = Binop.of_int (choice - nterms - Unop.number) in 
                    random_binop ~op:op t.bitsz
                else 
                    let op = Triop.of_int (choice - nterms - Unop.number - Binop.number) in 
                    random_triop ~op:op t.bitsz *)

        and mutate_unop  t = match t.node with
        | Unop (_, t') ->
            let choice = match get_random_left_choice t indice with 
            | Some v -> v
            | None -> raise NOCHOICE
            in
            (*if is_terminal t' && choice = 0 then*)
            begin match choice with
            | OP_Const(c) -> terminal_from_const c t.bitsz
            | OP_Var(v) -> terminal_from_var v t.bitsz
            | OP_Unop(op) -> mk_unop op t'
            | _ -> assert false
            end
            

            (*if 0 <= choice && choice < nconsts then 
                let op = (O.consts ()).(choice) in
                terminal_from_const op t.bitsz
            else if nconsts <= choice && choice < nterms  then 
                let op = (O.vars ()).(choice - nconsts) in
                terminal_from_var op t.bitsz
            else 
                let op = Unop.of_int (choice - nterms) in 
                mk_unop op t' *)
        | _ -> assert false

        and mutate_binop  t = match t.node with
        | Binop (_, t1, t2) ->
            let choice = match get_random_left_choice t indice with 
            | Some v -> v
            | None -> raise NOCHOICE
            in
            (*if is_terminal t1 && is_terminal t2 && choice = 0 then*)
            begin match choice with
            | OP_Const(c) -> terminal_from_const c t.bitsz
            | OP_Var(v) -> terminal_from_var v t.bitsz
            | OP_Binop(op) -> mk_binop op t1 t2
            | _ -> assert false
            end
            (*
            let nconsts = O.nconsts () in
            let nvars = O.nvars () in
            let nterms = nvars + nconsts in
            if 0 <= choice && choice < nconsts then 
                let op = (O.consts ()).(choice) in
                terminal_from_const op t.bitsz
            else if nconsts <= choice && choice < nterms  then 
                let op = (O.vars ()).(choice - nconsts) in
                terminal_from_var op t.bitsz
            else 
                let op = Binop.of_int (choice - nterms) in 
                mk_binop op t1 t2*)
        | _ -> assert false

        and mutate_triop  t = match t.node with
        | Triop (_, t1, t2, t3) -> 
            let choice = match get_random_left_choice t indice with 
            | Some v -> v
            | None -> raise NOCHOICE
            in
            (*if is_terminal t1 && is_terminal t2  && is_terminal t3 && choice = 0 then*)
            begin match choice with
            | OP_Const(c) -> terminal_from_const c t.bitsz
            | OP_Var(v) -> terminal_from_var v t.bitsz
            | OP_Triop(op) -> mk_triop op t1 t2 t3
            | _ -> assert false
            end
            (*
            let nconsts = O.nconsts () in
            let nvars = O.nvars () in
            let nterms = nvars + nconsts in
            if 0 <= choice && choice < nconsts then 
                let op = (O.consts ()).(choice) in
                terminal_from_const op t.bitsz
            else if nconsts <= choice && choice < nterms  then 
                let op = (O.vars ()).(choice - nconsts) in
                terminal_from_var op t.bitsz
            else 
                let op = Triop.of_int (choice - nterms) in 
                mk_triop op t1 t2 t3 *)
        | _ -> assert false
        in

        let rec recmutate t indice max allowed_nodes = 
          assert(max >= 0);
          let index = Random.int (Array.length allowed_nodes) in
          let rand_node = allowed_nodes.(index) in 
          try 
              aux 0 rand_node t indice
          with 
            | NOCHOICE -> 
                if max = 0  || (Array.length allowed_nodes) = 1 then
                    t
                else
                    let new_allowed = Array.init ((Array.length allowed_nodes)-1) (
                        fun i -> if i < index then allowed_nodes.(i) else allowed_nodes.(i+1)
                    )
                    in
                    recmutate t indice (max-1) new_allowed
        in
        let allowed_nodes = Array.init t.sz ((+) 1) in
        recmutate t indice t.sz allowed_nodes

    let cut t =
        let rec aux id t = match t.node with
        | Var _ | Const _ -> random_terminal t.bitsz
        | Unop (op, t') ->
            if id = t.sz then random_terminal t.bitsz
            else mk_unop op (aux id t')
        | Binop (op, t1, t2) ->
            let rank = 1 + t1.sz in
            if id = rank then random_terminal t.bitsz
            else if id < rank then mk_binop op (aux id t1) t2
            else mk_binop op t1 (aux (id - rank) t2)
        | Triop (op, t1, t2, t3) ->
            let rank = 1 + t1.sz in
            if id = rank then random_terminal t.bitsz
            else if id < rank then mk_triop op (aux id t1) t2 t3
            else if id > rank + t2.sz then mk_triop op t1 t2 (aux (id - (rank + t2.sz)) t3)
            else mk_triop op t1 (aux (id - rank) t2) t3
        in
        aux (1 + Random.int t.sz) t

    let eval t = t.vals

    let singleton () : t =
        let { sz ; _ } : Oracle.variable = O.out_var () in
        random_terminal sz

    let to_string (t : t) =
        let open Printf in
        let rec aux t = match t.node with
        | Var l -> sprintf "%s<%d>" l.name l.sz
        | Const c -> sprintf "%s" c.name
        | Unop (op, t) -> sprintf "%s (%s)" (Unop.to_string op) (aux t)
        | Binop (op, t1, t2) ->
            sprintf "(%s %s %s)" (aux t1) (Binop.to_string op) (aux t2)
        | Triop (op, t1, t2, t3) ->
                begin match op with
                | Div | SDiv | Mod | SMod -> sprintf "((%s ++ %s) %s %s)" (aux t1) (aux t2) (Triop.to_string op) (aux t3)
                | ITE | Im -> sprintf "%s(%s, %s, %s)" (Triop.to_string op) (aux t1) (aux t2) (aux t3)
                end
        in
        aux t

    let to_smtlib (t : t) =
        let open Printf in
        let rec aux t = match t.node with
        | Var l -> l.name
        | Const c -> 
                let hexstr = (Bitvector.to_hexstring c.value) in
                sprintf "#x%s"  (String.sub hexstr 2 ((String.length hexstr) -2))
        | Unop (op, t) -> 
                begin match op with
                | Extend sz -> sprintf "((_ zero_extend %d) %s)" (sz - t.bitsz) (aux t)
                | Sextend sz -> sprintf "((_ sign_extend %d) %s)" (sz - t.bitsz) (aux t)
                | Byteswap -> 
                    let e = aux t in
                    let sz = t.bitsz in
                    let rec loop i =
                        if i+8 == sz then
                            sprintf "((_ extract %d %d) %s)" (i+8-1) i e
                        else
                            sprintf "(concat ((_ extract %d %d) %s) %s)" (i+8-1) i e (loop (i+8))
                    in
                    loop 0
                | Ehad -> sprintf "(bvlshr %s %s)" (aux t) (cst_to_smtlib 1 t.bitsz)
                | Arba -> sprintf "(bvlshr %s %s)" (aux t) (cst_to_smtlib 4 t.bitsz)
                | Shesh -> sprintf "(bvlshr %s %s)" (aux t) (cst_to_smtlib 16 t.bitsz)
                | Smol -> sprintf "(bvshl %s %s)" (aux t) (cst_to_smtlib 1 t.bitsz)
                | _ -> sprintf "(%s %s)" (Unop.to_smtlib op) (aux t)
                end
        | Binop (op, t1, t2) ->
                begin match op with
                | RotateRight -> sprintf "((_ %s %s) %s)" (Binop.to_smtlib op) (aux t2) (aux t1) 
                | _ -> sprintf "(%s %s %s)" (Binop.to_smtlib op) (aux t1) (aux t2)
                end
        | Triop (op, t1, t2, t3) ->
                let catsize = t1.bitsz + t2.bitsz in
                let size = t3.bitsz in
                begin match op with
                | Div | Mod ->
                    sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ zero_extend %d) %s)))" (size-1) (Triop.to_smtlib op) (aux t1) (aux t2) (catsize - size) (aux t3)
                | SDiv | SMod ->
                    sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ sign_extend %d) %s)))" (size-1) (Triop.to_smtlib op) (aux t1) (aux t2) (catsize - size) (aux t3)
                | ITE -> sprintf "(%s (distinct  %s (_ bv0 %d)) %s %s)" (Triop.to_smtlib op) (aux t1) t1.bitsz (aux t2) (aux t3)
                | Im -> sprintf "(ite (= %s %s) %s %s)" (aux t1) (cst_to_smtlib 1 t1.bitsz) (aux t2) (aux t3)
                end
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
            let rec aux d id t = match t.node with
            | Var _ | Const _ -> mutate_leaf d t
            | Unop (op, t') ->
                if is_terminal t then mutate_leaf d t
                else if Unop.is_sz_modifier op then aux d id t'
                else if id = t.sz then mutate_unop t
                else let (t', r) = aux (d + 1) id t' in (mk_unop op t', r)
            | Binop (op, t1, t2) ->
                let rank = 1 + t1.sz in
                if id = rank then mutate_binop t
                else if id < rank then
                    let (t1', r) = aux (d + 1) id t1 in (mk_binop op t1' t2, r)
                else
                    let (t2', r) = aux (d + 1) (id - rank) t2 in (mk_binop op t1 t2', r)
            | Triop (_ ,_ ,_ ,_) -> assert false

            and mutate_leaf d t =
                let choice = Random.int 3 in
                if d = height_lim || choice = 0 then (random_terminal t.bitsz, 1.)
                else if choice = 1 then (random_unop t.bitsz, leaf_unop)
                else (random_binop t.bitsz, leaf_binop)

            and mutate_unop t = match t.node with
            | Unop (_, t') ->
                if is_terminal t' && Random.bool () then
                    (random_terminal t.bitsz, unop_leaf)
                else (mk_unop (Unop.rand ()) t', 1.)
            | _ -> assert false

            and mutate_binop t = match t.node with
            | Binop (_, t1, t2) ->
                if is_terminal t1 && is_terminal t2 && Random.bool () then
                    (random_terminal t.bitsz, binop_leaf)
                else (mk_binop (Binop.rand ()) t1 t2, 1.)
            | _ -> assert false
            in

            let (t', r) = aux 0 (1 + Random.int t.sz) t in
            (t', r *. float t.sz /. float t'.sz)

    let setvar tree (var : Oracle.variable) const =                                                                                                                                                                
        let rec aux t = match t.node with
        | Var v when String.equal var.name v.name -> mk_const const
        | Var _ -> t
        | Const _ -> t
        | Unop(op, t1) ->  mk_unop op (aux t1) 
        | Binop(op, t1, t2) ->  mk_binop op (aux t1) (aux t2) 
        | Triop(op, t1, t2, t3) ->  mk_triop op (aux t1) (aux t2) (aux t3)
        in
        aux tree

    let minus (x: Oracle.constant) = O.const_of_bitv (Unop.apply Minus x.value)
    let b_not (x: Oracle.constant) = O.const_of_bitv (Unop.apply Not x.value)
    let bswap (x: Oracle.constant) = O.const_of_bitv (Unop.apply Byteswap x.value)
    let sum (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Add x.value y.value)
    let sub (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Sub x.value y.value)
    let mul (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Mul x.value y.value)
    let b_and (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply And x.value y.value)
    let b_or (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Or x.value y.value)
    let b_xor (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Xor x.value y.value)
    let lshift (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply LShift x.value y.value)
    let rlshiftu (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply RShiftu x.value y.value)
    let rlshifts (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply RShifts x.value y.value)
    let ror (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply RotateRight x.value y.value)

    let div (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply Div x.value y.value z.value)
    let sdiv (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply SDiv x.value y.value z.value)
    let umod (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply Mod x.value y.value z.value)
    let smod (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply SMod x.value y.value z.value)
    let ite (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply ITE x.value y.value z.value)
    let im (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply Im x.value y.value z.value)

    (* @TODO: As the information inside the nodes are not used anymore,
       we could transform the tree into a syntax tree (only operator and operand) without the 
       type t record.
       This would make this function much simpler and avoid reconstructing useless record each time we perform an
       optimization 
    *)
    let rec simplify tree =
        let triop_fct = function
        | Div -> div
        | SDiv -> sdiv
        | Mod -> umod
        | SMod -> smod
        | ITE -> ite
        | Im -> im
        in

        let rec default_triop ty t1 t2 t3 =
            let res1, modif1 = aux t1 in
            let res2, modif2 = aux t2 in
            let res3, modif3 = aux t3 in
            (mk_triop ty res1 res2 res3, (modif1 || modif2 || modif3))
        
        and default_binop ty t1 t2 =
            let res1, modif1 = aux t1 in
            let res2, modif2 = aux t2 in
            (mk_binop ty res1 res2, (modif1 || modif2))
        
        and default_unop ty t1 =
            let res1, modif1 = aux t1 in
            (mk_unop ty res1, modif1)

        and aux t = 
            begin match t.node with

            (* ITE optimizations *)
            | Triop(ITE, { node = Const c; _ }, t2, t3) ->
                if Bitvector.is_zeros c.value then t3,true else t2,true
            | Triop(ITE, ({ node = Var v; _ } as t1), t2, t3) ->
                let res2, modif2 = aux t2 in
                let res3, modif3 = aux (setvar t3 v (O.const_of_int 0 t3.bitsz)) in
                (mk_triop ITE t1 res2 res3, (modif2 || modif3))
            | Triop(ITE, t1, t2, t3) -> default_triop ITE t1 t2 t3

            (* Triop constant optimizations *)
            | Triop (ty, {node = Const c1; _}, {node = Const c2; _}, {node = Const c3;_}) ->
                (mk_const ((triop_fct ty) c1 c2 c3), true)

            (* Div optimizations *)
            | Triop(Div, t1, t2, t3) -> 
                    begin match t1.node, t2.node, t3.node with
                    | _, _, Const c when Bitvector.is_zeros c.value -> (mk_const (O.const_of_bitv (Bitvector.max_ubv (Bitvector.size_of c.value))), true)
                    | _, _, Const c when Bitvector.is_ones c.value -> 
                            (t2, true) (* only t2 because we reduce to size bits*)
                    | _, _, _ -> default_triop Div t1 t2 t3
                    end
            (* SDiv optimizations *)
            | Triop(SDiv, t1, t2, t3) -> 
                    begin match t1.node, t2.node, t3.node with
                    | _, _, Const c when Bitvector.is_ones c.value -> 
                            (t2, true) (* only t2 because we reduce to size bits*)
                    | _, _, _ -> default_triop SDiv t1 t2 t3   
                    end
            
            (* Mod optimizations *)
            | Triop(Mod, t1, t2, t3) ->
                    begin match t1.node, t2.node, t3.node with
                    | _, _, Const c when Bitvector.is_ones c.value -> (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | _, _, _ when (tree_equal t1 t2) && (tree_equal t1 t3) ->
                            (mk_const (O.const_of_int 0 (t1.bitsz)), true)
                    | _, _, _ -> default_triop Mod t1 t2 t3
                    end
            
            (* SMod optimizations *)
            | Triop(SMod, t1, t2, t3) ->
                    begin match t1.node, t2.node, t3.node with
                    | _, _, Const c when Bitvector.is_ones c.value -> (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | _, _, _ -> default_triop SMod t1 t2 t3
                    end

            (* Default case (for later)
            | Triop(ty, t1, t2, t3) -> default_triop ty t1 t2 t3*)

            (* Add optimizations *)
            | Binop(Add , t1, t2) -> 
                    begin match t1.node, t2.node with
                    | Const c1, Const c2 -> (mk_const (sum c1 c2), true)
                    | _ , Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (t2, true)
                    | Binop(Add, t1', t2'), Const c
                    | Const c, Binop(Add, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sum c2 (sum c1 c)), true)
                            | _ , Const c1 -> (mk_binop Add t1' (mk_const (sum c c1)), true)
                            | Const c1, _ -> (mk_binop Add t2' (mk_const (sum c c1)), true)
                            | _ , _ -> default_binop Add t1 t2
                            end

                    | Binop(Sub, t1', t2'), Const c
                    | Const c, Binop(Sub, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sum (sub c1 c2) c), true)
                            | _ , Const c1 -> (mk_binop Add t1' (mk_const (sub c c1)), true)
                            | Const c1, _ -> (mk_binop Sub (mk_const (sum c c1)) t2', true)
                            | _ , _ -> default_binop Add t1 t2
                            end
                    | Binop(Xor, t11', t12'), Binop(And, t21', t22') 
                            when (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') ->
                                mk_binop Or t11' t12', true
                    | Binop(And, t11', t12'), Binop(Xor, t21', t22') 
                            when (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') ->
                                mk_binop Or t11' t12', true

                    | Binop(Sub, t11', t12'), _ when tree_equal t12' t2 ->
                            t11', true
                    | _, Binop(Sub, t21', t22') when tree_equal t22' t1 ->
                            t21', true


                    | Unop(Minus, t1'), _ -> (mk_binop Sub t2 t1', true)
                    | _, Unop(Minus, t2') -> (mk_binop Sub t1 t2', true)
                    | Unop(Not, t1') , _ when tree_equal t1' t2 -> 
                            (mk_unop Minus (mk_const (O.const_of_int 1 t1'.bitsz)), true)
                    | _, Unop(Not, t2') when tree_equal t1 t2' -> 
                            (mk_unop Minus (mk_const (O.const_of_int 1 t2'.bitsz)), true)
                    | Unop(Not, t1'), Const c when Bitvector.is_ones c.value ->
                            (mk_unop Minus t1', true)
                    | Const c, Unop(Not, t1') when Bitvector.is_ones c.value ->
                            (mk_unop Minus t1', true)
                    | Unop(Not, t1'), Binop(Add, t21', t22') when tree_equal t1' t21' ->
                            mk_binop Sub t22' (mk_const (O.const_of_int 1 t22'.bitsz)) , true
                    | Unop(Not, t1'), Binop(Add, t21', t22') when tree_equal t1' t22' ->
                            mk_binop Sub t21' (mk_const (O.const_of_int 1 t21'.bitsz)) , true
                    | Var v1, Var v2 when v1 = v2 -> (mk_binop Mul (mk_const (O.const_of_int 2 v2.sz)) t1, true)
                    | _, _ -> default_binop Add t1 t2
                    end

            (* Mul optimizations *)
            | Binop(Mul , t1, t2) -> 
                    begin match t1.node, t2.node with
                    | Const c1, Const c2 -> (mk_const (mul c1 c2), true)
                    | _ , Const c when Bitvector.is_zeros c.value ->
                            (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | _ , Const c when Bitvector.is_ones c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_ones c.value ->
                            (t2, true)
                    | Const c, _ when Bitvector.is_max_ubv c.value ->
                            (mk_unop Minus t2, true)
                    | _, Const c when Bitvector.is_max_ubv c.value ->
                            (mk_unop Minus t1, true)
                    | Binop(Mul, t1', t2'), Const c
                    | Const c, Binop(Mul, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (mul c2 (mul c1 c)), true)
                            | _ , Const c1 -> (mk_binop Mul t1' (mk_const (mul c c1)), true)
                            | Const c1, _ -> (mk_binop Mul t2' (mk_const (mul c c1)), true)
                            | _ , _ -> default_binop Mul t1 t2
                            end
                    | Unop(Minus, t1'), Unop(Minus, t2') -> (mk_binop Mul t1' t2', true)
                    | Unop(Minus, t1'), _ -> (mk_unop Minus (mk_binop Mul t1' t2), true)
                    | _, Unop(Minus, t2') -> (mk_unop Minus (mk_binop Mul t1 t2'), true)
                    | _, _ -> default_binop Mul t1 t2
                    end

            (* Sub optimizations *)
            | Binop(Sub , t1, t2) -> 
                    begin match t1.node, t2.node with
                    | _, _ when tree_equal t1 t2 -> (mk_const (O.const_of_int 0 t1.bitsz), true)
                    | Const c1, Const c2 -> (mk_const (sub c1 c2), true) (*TODO if c1 < c2 create unop Minus ...*)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (mk_unop Minus t2, true)
                    | Binop(Sub, t1', t2'), Const c ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sub (sub c1 c2) c), true)
                            | _ , Const c1 -> (mk_binop Sub t1' (mk_const (sum c c1)), true)
                            | Const c1, _ -> (mk_binop Sub (mk_const (sub c1 c)) t2', true)
                            | _ , _ -> default_binop Sub t1 t2
                            end
                    | Const c, Binop(Sub, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sub c (sub c1 c2)), true)
                            | _ , Const c1 -> (mk_binop Sub (mk_const (sum c c1)) t1', true)
                            | Const c1, _ -> (mk_binop Add (mk_const (sub c c1)) t2', true)
                            | _ , _ -> default_binop Sub t1 t2
                            end
                    | Const c, Binop(Add, t1', t2') -> 
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2  -> (mk_const (sub c (sum c1 c2)), true) 
                            | Const c1, _ -> (mk_binop Sub (mk_const (sub c c1)) t2', true)
                            | _, Const c2 -> (mk_binop Sub (mk_const (sub c c2)) t1', true)
                            | _, _ -> default_binop Sub t1 t2
                            end
                    | Binop(Add, t1', t2'), Const c -> 
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2  -> (mk_const (sub (sum c1 c2) c), true) 
                            | Const c1, _ -> (mk_binop Sub t2' (mk_const (sub c c1)), true)
                            | _, Const c2 -> (mk_binop Sub t1' (mk_const (sub c c2)), true)
                            | _, _ -> default_binop Sub t1 t2
                            end
                    | Unop(Minus, t1'), Const c when Bitvector.is_ones c.value ->
                            mk_unop Not t1', true
                    | _, Unop(Minus, t2') -> (mk_binop Add t1 t2', true) 
                    | Unop(Minus, t1'), _ -> (mk_unop Minus (mk_binop Add t1' t2), true) 
                    | Var v1, Var v2 when v1 = v2 -> (mk_const (O.const_of_int 0 v1.sz), true)
                    | _, _ -> 
                            (*
                             * if t1 = t2 then 0 else
                             *)
                            default_binop Sub t1 t2
                    end
            
            (* LShift optimizations *)
            | Binop(LShift, { node = Const c1; _ }, { node = Const c2; _ }) ->
                (mk_const (lshift c1 c2), true)
            (* RShiftu optimizations *)
            | Binop(RShiftu, { node = Const c1; _ }, { node = Const c2; _ }) ->
                (mk_const (rlshiftu c1 c2), true)
            (* RShifts optimizations *)
            | Binop(RShifts, { node = Const c1; _ }, { node = Const c2; _ }) ->
                (mk_const (rlshifts c1 c2), true)
            | Binop(RotateRight, { node = Const c1; _ }, { node = Const c2; _ }) ->
                (mk_const (ror c1 c2), true)
            
            (* Generic Shift optimization (x shift 0 = x) *)
            | Binop(LShift, t1, { node = Const c; _ })
            | Binop(RShiftu, t1, { node = Const c; _ })
            | Binop(RShifts, t1, { node = Const c; _ }) when Bitvector.is_zeros c.value ->
                (t1, true)
            
            (* Default case (to merge with default binop) *)
            | Binop(LShift as ty, t1, t2) 
            | Binop(RShiftu as ty, t1, t2)
            | Binop(RShifts as ty, t1, t2) -> 
                default_binop ty t1 t2

            (* And optimizations *)
            | Binop(And, t1, t2) ->
                    begin match t1.node, t2.node with
                    | _, _ when tree_equal t1 t2 -> (t1, true) 
                    | Const c1, Const c2 -> (mk_const (b_and c1 c2), true) 
                    | _, Const c when Bitvector.is_max_ubv c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_max_ubv c.value ->
                            (t2, true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | Const c, Binop(And, t1', t2') 
                    | Binop(And, t1', t2'), Const c ->
                            begin match t1'.node, t2'.node with
                            | Const c1', Const c2' -> (mk_const (b_and c (b_and c1' c2')), true) 
                            | Const c', _ -> (mk_binop And t2' (mk_const (b_and c c')), true)
                            | _, Const c' -> (mk_binop And t1' (mk_const (b_and c c')), true)
                            | _, _ -> default_binop And t1 t2
                            end
                    | Const c, Binop(Mul, t21', t22') when Bitvector.is_ones c.value ->
                            begin match t21'.node, t22'.node with
                            | Const c, _ when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, Const c when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, _ -> default_binop And t1 t2
                            end
                    | Binop(Mul, t11', t12'), Const c when Bitvector.is_ones c.value ->
                            begin match t11'.node, t12'.node with
                            | Const c, _ when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, Const c when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, _ -> default_binop And t1 t2
                            end
                    | Binop(Xor, t11', t12'), Binop(And, t21', t22') 
                            when (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') ->
                                mk_const (O.const_of_int 0 t11'.bitsz), true
                    | Binop(And, t11', t12'), Binop(Xor, t21', t22') 
                            when (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') ->
                                mk_const (O.const_of_int 0 t11'.bitsz), true
                    | _ , Unop(Not, t2') when tree_equal t1 t2' ->
                            (mk_const (O.const_of_int 0 t2'.bitsz), true)
                    | Unop(Not, t1'), _ when tree_equal t1' t2 ->
                            (mk_const (O.const_of_int 0 t1'.bitsz), true)

                    | Unop(Minus, t'), _ ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value -> (t2, true)
                            | _ -> default_binop And t1 t2
                            end
                    | _ , Unop(Minus, t') ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value -> (t1, true)
                            | _ -> default_binop And t1 t2
                            end

                    | Binop(And, t1', t2'), _ when tree_equal t1' t2 || tree_equal t2' t2 -> 
                            (t1, true)
                    | _, Binop(And, t1', t2') when tree_equal t1' t1 || tree_equal t2' t1 -> 
                            (t2, true)

                    | Binop(And, t11', t12'), Binop(And, t21', t22') when tree_equal t11' t21' ->
                            mk_binop And (mk_binop And t11' t12') t22', true
                    | Binop(And, t11', t12'), Binop(And, t21', t22') when tree_equal t11' t22' ->
                            mk_binop And (mk_binop And t11' t12') t21', true
                    | Binop(And, t11', t12'), Binop(And, t21', t22') when tree_equal t12' t21' ->
                            mk_binop And (mk_binop And t11' t12') t22', true
                    | Binop(And, t11', t12'), Binop(And, t21', t22') when tree_equal t12' t22' ->
                            mk_binop And (mk_binop And t11' t12') t21', true


                    | Binop(And, t11', t12'), Unop(Not, t2') when tree_equal t11' t2' || tree_equal t12' t2' -> 
                            (mk_const (O.const_of_int 0 t11'.bitsz), true)
                    | Unop(Not, t1'), Binop(And, t21', t22') when tree_equal t1' t21' || tree_equal t1' t22' -> 
                            (mk_const (O.const_of_int 0 t1'.bitsz), true)
                    | _, Binop(And, t21', t22') -> 
                            begin match t21'.node, t22'.node with
                            | Unop(Not, t21''), _ when tree_equal t1 t21'' -> 
                                    (mk_const (O.const_of_int 0 t21''.bitsz), true)
                            | _, Unop(Not, t22'') when tree_equal t1 t22'' -> 
                                    (mk_const (O.const_of_int 0 t22''.bitsz), true)
                            | _, _ -> default_binop And t1 t2
                            end
                    | _, _ -> default_binop And t1 t2
                    end
            
            (* Or optimizations *)
            | Binop(Or, t1, t2) -> 
                    begin match t1.node, t2.node with
                    | _, _ when tree_equal t1 t2 -> (t1, true) 
                    | Const c1, Const c2 -> (mk_const (b_or c1 c2), true) 
                    | _, Const c when Bitvector.is_max_ubv c.value ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 (Bitvector.size_of c.value))), true)
                    | Const c, _ when Bitvector.is_max_ubv c.value ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 (Bitvector.size_of c.value))), true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (t2, true)
                    | Binop(Or, t1', t2'), Const c 
                    | Const c, Binop(Or, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1', Const c2' -> (mk_const (b_or c (b_or c1' c2')), true)
                            | Const c', _ -> (mk_binop Or t2' (mk_const (b_or c' c)), true)
                            | _, Const c' -> (mk_binop Or t1' (mk_const (b_or c' c)), true)
                            | _, _ -> default_binop Or t1 t2
                            end
                    | Binop(Xor, t1', t2'), _ when tree_equal t1' t2 || tree_equal t2' t2 ->
                            (mk_binop Or t1' t2', true)
                    | _, Binop(Xor, t1', t2') when tree_equal t1' t1 || tree_equal t2' t1 ->
                            (mk_binop Or t1' t2', true)
                            
                    | _ , Unop(Not, t2') when tree_equal t1 t2' ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 t2'.bitsz)), true)
                    | Unop(Not, t1'), _ when tree_equal t1' t2 ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 t1'.bitsz)), true)
                    | Unop(Minus, t'), _ | _ , Unop(Minus, t') ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value ->
                                (mk_unop Minus (mk_const (O.const_of_int 1 (Bitvector.size_of c.value))), true)
                            | _ -> default_binop Or t1 t2
                            end
                    | Binop(And, t1', t2'), _ when tree_equal t1' t2 || tree_equal t2' t2 ->
                            (t2, true)
                    | _, Binop(And, t1', t2') when tree_equal t1 t1' || tree_equal t1 t2' ->
                            (t1, true)

                    | Binop(Or, t1', t2'), _ when tree_equal t1' t2 || tree_equal t2' t2 -> 
                                (t1, true)
                    | _, Binop(Or, t1', t2') when tree_equal t1' t1 || tree_equal t2' t1 -> 
                                (t2, true)
                    | Binop(Or, t11', t12'), Binop(Or, t21', t22') when tree_equal t11' t21' ->
                            mk_binop Or (mk_binop Or t11' t12') t22', true
                    | Binop(Or, t11', t12'), Binop(Or, t21', t22') when tree_equal t11' t22' ->
                            mk_binop Or (mk_binop Or t11' t12') t21', true
                    | Binop(Or, t11', t12'), Binop(Or, t21', t22') when tree_equal t12' t21' ->
                            mk_binop Or (mk_binop Or t11' t12') t22', true
                    | Binop(Or, t11', t12'), Binop(Or, t21', t22') when tree_equal t12' t22' ->
                            mk_binop Or (mk_binop Or t11' t12') t21', true

                    | Binop(Or, t11', t12'), Unop(Not, t21') when tree_equal t11' t21' || tree_equal t12' t21' -> 
                                (mk_unop Minus (mk_const (O.const_of_int 1 t11'.bitsz)), true)
                    | Unop(Not, t11'), Binop(Or, t21', t22') when tree_equal t11' t21' || tree_equal t11' t22' -> 
                                (mk_unop Minus (mk_const (O.const_of_int 1 t11'.bitsz)), true)
                    | _, _ -> default_binop Or t1 t2
                    end
            
            (* Xor optimizations *)
            | Binop(Xor, t1, t2) -> 
                    begin match t1.node, t2.node with
                    | _, _ when tree_equal t1 t2 -> (mk_const (O.const_of_int 0 t1.bitsz), true) 
                    | _, Unop(Not, t2') when tree_equal t1 t2' ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 t2'.bitsz)), true)
                    | Unop(Not, t1'), _ when tree_equal t1' t2 ->
                            (mk_unop Minus (mk_const (O.const_of_int 1 t1'.bitsz)), true)
                    | Const c1, Const c2 -> (mk_const (b_xor c1 c2), true) 
                    | _, Const c when Bitvector.is_max_ubv c.value ->
                            (mk_unop Not t1, true)
                    | Const c, _ when Bitvector.is_max_ubv c.value ->
                            (mk_unop Not t2, true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | Const c, _ when Bitvector.is_zeros c.value ->
                            (t2, true)
                    | Binop(Xor, t1', t2'), _ when tree_equal t1' t2 -> (t2', true)
                    | Binop(Xor, t1', t2'), _ when tree_equal t2' t2 -> (t1', true)
                    | _, Binop(Xor, t1', t2') when tree_equal t1' t1 -> (t2', true)
                    | _, Binop(Xor, t1', t2') when tree_equal t2' t1 -> (t1', true)
                    | Binop(Xor, t11', t12'), Binop(Xor, t21', t22') when tree_equal t11' t21' ->
                            mk_binop Xor t12' t22', true
                    | Binop(Xor, t11', t12'), Binop(Xor, t21', t22') when tree_equal t11' t22' ->
                            mk_binop Xor t12' t21', true
                    | Binop(Xor, t11', t12'), Binop(Xor, t21', t22') when tree_equal t12' t21' ->
                            mk_binop Xor t11' t22', true
                    | Binop(Xor, t11', t12'), Binop(Xor, t21', t22') when tree_equal t12' t22' ->
                            mk_binop Xor t11' t21', true

                    | Unop(Minus, t21'), Binop(Or, t11', t12') (* simplify -(1 | e) ^ (1 | e)  -> -2*)
                    | Binop(Or, t11', t12'), Unop(Minus, t21') -> (* simplify (1 | e) ^ -(1 | e)  -> -2*)
                            begin match t11'.node, t12'.node, t21'.node with
                            | Const c, _, Binop(Or, ch1, ch2) when Bitvector.get_bit c.value 0 -> 
                                    begin match ch1.node, ch2.node with
                                    | _, Const c' when Bitvector.get_bit c'.value 0 && (tree_equal ch1 t12') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | Const c', _ when Bitvector.get_bit c'.value 0 && (tree_equal ch2 t12') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | _, _ -> default_binop Xor t1 t2
                                    end

                            | _, Const c, Binop(Or, ch1, ch2) when Bitvector.get_bit c.value 0 -> 
                                    begin match ch1.node, ch2.node with
                                    | _, Const c' when Bitvector.get_bit c'.value 0 && (tree_equal ch1 t11') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | Const c', _ when Bitvector.get_bit c'.value 0 && (tree_equal ch2 t11') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | _, _ -> default_binop Xor t1 t2
                                    end
                            | _, _, _ -> default_binop Xor t1 t2
                            end

                    | _ , Unop(Minus, t') ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value ->
                                (mk_unop Not t1, true)
                            | _ -> default_binop Xor t1 t2
                            end
                    | Unop(Minus, t'), _  ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value ->
                                (mk_unop Not t2, true)
                            | _ -> default_binop Xor t1 t2
                            end
                    | _, _ -> default_binop Xor t1 t2
                    end
            
            (* Minus optimizations *)
            | Unop(Minus, t1) -> 
                    begin match t1.node with
                    | Const c -> (mk_const (minus c), true)
                    | Unop(Minus, t1') -> (t1', true)
                    | Binop(Add, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | _, Const c when Bitvector.is_ones c.value ->
                                    mk_unop Not t1', true
                            | Const c, _ when Bitvector.is_ones c.value ->
                                    mk_unop Not t2', true
                            | _, _ -> default_unop Minus t1
                            end
                    | _ -> default_unop Minus t1
                    end
            (* Not optimizations *)
            | Unop(Not, t1) -> 
                    begin match t1.node with
                    | Const c -> (mk_const (b_not c), true)
                    | Unop(Not, t1') -> (t1', true)
                    | _ -> default_unop Not t1
                    end
            (* Byteswap optimization *)
            | Unop(Byteswap, t1) ->
                begin match t1.node with
                | Const c -> (mk_const (bswap c), true)
                | Unop(Byteswap, t1') -> (t1', true)
                | _ -> default_unop Byteswap t1
                end
            | _ -> (t, false)
            end
        in
        begin match (aux tree) with
        | tree, true -> simplify tree
        | tree, false -> tree
        end
    
    let get_expr_size t = t.sz
    let get_mutations () = (module M : MUTATIONS)
end
