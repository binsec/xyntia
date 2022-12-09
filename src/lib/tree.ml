(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2019-2022                                               *)
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

let select omega =
    omega.(Random.int (Array.length omega))

type unop_t = Minus | Not | Extend of int | Sextend of int | Reduce of int

module type UNOP = sig
    val rand : unit -> unop_t
    val cardinal : unit -> int
    val apply : unop_t -> Bitvector.t -> Bitvector.t
    val to_string : unop_t -> string
    val to_smtlib : unop_t -> string
    val is_sz_modifier : unop_t -> bool
end

module Mk_Unop (Oracle : Oracle.ORACLE) : UNOP = struct
    let number = 2
    let of_string = function
    | "not" -> Some Not
    | "neg" -> Some Minus
    | _ -> None

    let of_int = function
    | 0 -> Minus
    | 1 -> Not
    | _ -> assert false

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let unops = List.filter
                (function Some _ -> true | None -> false)
                (List.map of_string ops)
            in
            let unops = Array.of_list
                (List.map (
                    function
                    | Some x -> x
                    | _ -> assert false) unops)
            in
            if Array.length unops = 0 then [| Minus |] else unops

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

    let to_string = function
    | Minus -> "-"
    | Not -> "not"
    | Extend sz -> Printf.sprintf "%dext" sz
    | Sextend sz -> Printf.sprintf "%dsext" sz
    | Reduce sz -> Printf.sprintf "%dred" sz

    let to_smtlib = function
    | Minus -> "bvneg"
    | Not -> "bvnot"
    | Extend sz -> Printf.sprintf "(_ zero_extend %d)" sz
    | Sextend sz -> Printf.sprintf "(_ sign_extend %d)" sz
    | Reduce sz -> Printf.sprintf "(_ extract %d 0)" (sz-1)
end

type binop_t = Add | Sub | Mul | And | Or | Xor | RShiftu | LShift | RShifts

module type BINOP = sig
    val rand : unit -> binop_t
    val cardinal : unit -> int
    val apply : binop_t -> Bitvector.t -> Bitvector.t -> Bitvector.t
    val to_string : binop_t -> string
    val to_smtlib : binop_t -> string
end

module Mk_Binop(Oracle : Oracle.ORACLE) : BINOP = struct
    let number = 9 (* number of operators*)

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
    | _ -> assert false

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let binops = List.filter
                (function Some _ -> true | None -> false)
                (List.map of_string ops)
            in
            let binops = Array.of_list
                (List.map (
                    function
                    | Some x -> x
                    | _ -> assert false
                ) binops)
            in
            if Array.length binops = 0 then [| Add |] else binops

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

    let rand () = select omega
    let cardinal () = Array.length omega

    let unsigned_int x size = (x + (1 lsl size)) mod (1 lsl size) 

    let apply op x y = 
        let _ = if (Bitvector.size_of x) != (Bitvector.size_of y) then failwith "Binop.apply: different sizes" in
        let size = Bitvector.size_of x in
        match op with
        | Add -> Bitvector.add x y
        | Sub -> Bitvector.sub x y
        | Mul -> Bitvector.umul x y
        | And -> Bitvector.logand x y
        | Or  -> Bitvector.logor x y
        | Xor -> Bitvector.logxor x y
        | RShiftu -> 
            let offset = unsigned_int (Bitvector.to_int y) (Bitvector.size_of y) in
            Bitvector.shift_right x (offset land (size -1)) 
        | LShift -> 
            let offset = unsigned_int (Bitvector.to_int y) (Bitvector.size_of y) in
            Bitvector.shift_left x (offset land (size -1)) 
        | RShifts -> 
            let offset = Bitvector.to_int y in 
            if offset < 0 && (Bitvector.sgt x (Bitvector.zeros size)) then 
                Bitvector.zeros size
            else if offset < 0 && (Bitvector.sle x (Bitvector.zeros size)) then 
                Bitvector.ones size
            else 
                Bitvector.shift_right_signed x (offset land (size -1))
end

type triop_t = Div | SDiv | Mod | SMod | ITE

module type TRIOP = sig
    val rand : unit -> triop_t
    val cardinal : unit -> int
    val apply : triop_t -> Bitvector.t -> Bitvector.t -> Bitvector.t -> Bitvector.t
    val to_string : triop_t -> string
    val to_smtlib : triop_t -> string
end

module Mk_Triop(Oracle : Oracle.ORACLE) : TRIOP = struct
    let number = 5
    let of_string = function
    | "div" -> Some Div
    | "sdiv" -> Some SDiv
    | "umod" -> Some Mod
    | "smod" -> Some SMod
    | "ite" -> Some ITE
    | _ -> None
    let of_int = function
    | 0 -> Div
    | 1 -> SDiv
    | 2 -> Mod
    | 3 -> SMod
    | 4 -> ITE
    | _ -> assert false 

    let omega =
        match Oracle.ops () with
        | None -> Array.init number of_int
        | Some ops ->
            let triops = List.filter
                (function Some _ -> true | None -> false)
                (List.map of_string ops)
            in
            let triops = Array.of_list
                (List.map (
                    function
                    | Some x -> x
                    | _ -> assert false
                ) triops)
            in
            if Array.length triops = 0 then [| Div |] else triops

    let to_string = function
    | Div -> "/"
    | SDiv -> "/s"
    | Mod -> "%"
    | SMod -> "%s"
    | ITE -> "if"

    let to_smtlib = function
    | Div -> "bvudiv"
    | SDiv -> "bvsdiv"
    | Mod -> "bvurem"
    | SMod -> "bvsrem"
    | ITE -> "ite"

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
                Bitvector.extract (Bitvector.udiv (Bitvector.concat [x; y]) (Bitvector.extend z catsize)) 0 (size -1)
        | SDiv -> 
            let cat = Bitvector.concat [x; y] in
            if Bitvector.is_zeros z then
                if Bitvector.sge cat (Bitvector.zeros catsize) then
                    Bitvector.max_ubv size
                else
                    Bitvector.ones size
            else
                Bitvector.extract (Bitvector.sdiv cat (Bitvector.extend_signed z catsize)) 0 (size-1)
        | Mod ->
            let cat = Bitvector.concat [x; y] in
            let divisor = Bitvector.extend z catsize in
            if Bitvector.is_zeros divisor then
                if Bitvector.equal cat divisor then
                    Bitvector.zeros size
                else
                    y
            else
                Bitvector.extract (Bitvector.umod cat divisor) 0 (size-1)

        | SMod -> 
            let cat = Bitvector.concat [x; y] in
            let divisor = Bitvector.extend_signed z catsize in
            if Bitvector.is_zeros divisor then
                y
            else
                Bitvector.extract (Bitvector.smod cat divisor) 0 (size-1)

        | ITE -> 
                if Bitvector.is_zeros x then
                    z
                else
                    y
end

type op_t = OP_Var of Oracle.variable | OP_Const of Oracle.constant | OP_Unop of unop_t | OP_Binop of binop_t | OP_Triop of triop_t

module type MUTATIONS = sig
    val all_mut : unit -> op_t array
    val unop_mut : unit -> op_t array
    val binop_mut : unit -> op_t array
    val triop_mut : unit -> op_t array
end

module Mk_Mutations_Five(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () = 
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

                OP_Unop(Minus);
                OP_Unop(Not);

                OP_Binop(Add);
                OP_Binop(Mul);
                OP_Binop(Or);
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

module Mk_Mutations_Mba(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () = 
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

                OP_Unop(Minus);
                OP_Unop(Not);

                OP_Binop(Add);
                OP_Binop(Sub);
                OP_Binop(Mul);
                OP_Binop(And);
                OP_Binop(Or);
                OP_Binop(Xor)
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

module Mk_Mutations_Mba_Shift(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () =
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

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
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

module Mk_Mutations_Mba_ITE(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () = 
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

                OP_Unop(Minus);
                OP_Unop(Not);

                OP_Binop(Add);
                OP_Binop(Sub);
                OP_Binop(Mul);
                OP_Binop(And);
                OP_Binop(Or);
                OP_Binop(Xor);

                OP_Triop(ITE)
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

module Mk_Mutations_Expr(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () = 
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

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
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

module Mk_Mutations_Full(O : Oracle.ORACLE) : MUTATIONS = struct
    let all_mut () = 
        let consts = Array.init (O.nconsts ()) (fun i -> OP_Const((O.consts ()).(i))) in
        let vars = Array.init (O.nvars ()) (fun i -> OP_Var((O.vars ()).(i))) in
        Array.concat [ consts ; vars; [|
                OP_Const(O.random_const ());
                OP_Var(O.random_var ());

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
        |] ]

    let unop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Unop(_) -> true
            | _ -> false
        ) l)

    let binop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Binop(_) -> true
            | _ -> false
        ) l)

    let triop_mut () = 
        let l = Array.to_list (all_mut ()) in
        Array.of_list (List.filter (fun e -> 
            match e with
            | OP_Const(_) | OP_Var(_) | OP_Triop(_) -> true
            | _ -> false
        ) l)
end

let mutations_of_str (module O: Oracle.ORACLE) str = match str with
| "5" -> (module Mk_Mutations_Five (O) : MUTATIONS)
| "mba" -> (module Mk_Mutations_Mba (O) : MUTATIONS)
| "expr" -> (module Mk_Mutations_Expr (O) : MUTATIONS)
| "mba_shift" -> (module Mk_Mutations_Mba_Shift (O) : MUTATIONS)
| "mba_ite" -> (module Mk_Mutations_Mba_ITE (O) : MUTATIONS)
| "full" -> (module Mk_Mutations_Full (O) : MUTATIONS)
| _ -> assert false

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
end

module Mk_Mutator(O : Oracle.ORACLE) (M : MUTATIONS) : MUTATOR = struct
    module Unop = Mk_Unop(O)
    module Binop = Mk_Binop(O)
    module Triop = Mk_Triop(O)

    exception NOCHOICE

    let all_mut = M.all_mut
    let unop_mut = M.unop_mut 
    let binop_mut = M.binop_mut 
    let triop_mut = M.triop_mut 

    let map3 f x y z =
        let lx = Array.length x in
        let ly = Array.length y in
        let lz = Array.length z in
        if lx <> ly || lx <> lz then
            assert false
        else begin
            if lx = 0 then [||] else begin
                let r = Array.make lx (f (Array.get x 0) (Array.get y 0) (Array.get z 0)) in
                for i = 1 to lx - 1 do
                    Array.set r i (f (Array.get x i) (Array.get y i) (Array.get z i))
                done;
                r
            end
        end

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
        vals = map3 (fun x y z -> Triop.apply op x y z) t1.vals t2.vals t3.vals;
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
        | Sub | LShift | RShifts | RShiftu ->
            (tree_equal t11' t21' && tree_equal t12' t22')
        | _ ->
            (tree_equal t11' t21' && tree_equal t12' t22') || (tree_equal t11' t22' && tree_equal t12' t21') 
        end
    | Triop(op1, t11', t12', t13'), Triop(op2, t21', t22', t23') when op1 = op2 -> 
        begin match op1 with
        | Div | SDiv | Mod | SMod | ITE ->
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
        let allowed_nodes = Array.init t.sz (fun i -> i+1) in
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
                | ITE -> sprintf "%s(%s, %s, %s)" (Triop.to_string op) (aux t1) (aux t2) (aux t3)
                end
        in
        aux t

    let to_smtlib (t : t) =
        let open Printf in
        let rec aux t = match t.node with
        | Var l -> sprintf "%s<%d>" l.name l.sz
        | Const c -> 
                let hexstr = (Bitvector.to_hexstring c.value) in
                sprintf "#x%s"  (String.sub hexstr 2 ((String.length hexstr) -2))
        | Unop (op, t) -> sprintf "(%s %s)" (Unop.to_smtlib op) (aux t)
        | Binop (op, t1, t2) ->
            sprintf "(%s %s %s)" (Binop.to_smtlib op) (aux t1) (aux t2)
        | Triop (op, t1, t2, t3) ->
                let catsize = t1.bitsz + t2.bitsz in
                let size = t3.bitsz in
                begin match op with
                | Div | Mod ->
                    sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ zero_extend %d) %s)))" (size-1) (Triop.to_smtlib op) (aux t1) (aux t2) (catsize - size) (aux t3)
                | SDiv | SMod ->
                    sprintf "((_ extract %d 0) (%s (concat %s %s) ((_ sign_extend %d) %s)))" (size-1) (Triop.to_smtlib op) (aux t1) (aux t2) (catsize - size) (aux t3)
                | ITE -> sprintf "(%s (distinct  %s (_ bv0 %d)) %s %s)" (Triop.to_smtlib op) (aux t1) t1.bitsz (aux t2) (aux t3)
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

    let sum (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Add x.value y.value)
    let sub (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Sub x.value y.value)
    let mul (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Mul x.value y.value)
    let b_and (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply And x.value y.value)
    let b_or (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Or x.value y.value)
    let b_xor (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply Xor x.value y.value)
    let lshift (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply LShift x.value y.value)
    let rlshiftu (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply RShiftu x.value y.value)
    let rlshifts (x: Oracle.constant) (y: Oracle.constant) = O.const_of_bitv (Binop.apply RShifts x.value y.value)

    let div (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply Div x.value y.value z.value)
    let sdiv (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply SDiv x.value y.value z.value)
    let umod (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply Mod x.value y.value z.value)
    let smod (x: Oracle.constant) (y: Oracle.constant) (z: Oracle.constant) = O.const_of_bitv (Triop.apply SMod x.value y.value z.value)

    let rec simplify tree =
        let rec aux t = 
            begin match t.node with
            | Triop(ITE, t1, t2, t3) ->                                                                                                                                                                            
                    begin match t1.node, t2.node, t3.node with
                    | Const c, _, _ ->
                            if Bitvector.is_zeros c.value then
                                t3, true
                            else
                                t2, true               
                    | Var v, _, _ ->
                            let res2, modif2 = aux t2 in               
                            let res3, modif3 = aux (setvar t3 v (O.const_of_int 0 t3.bitsz)) in          
                            (mk_triop ITE t1 res2 res3, (modif2 || modif3))        
                
                    | _, _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            let res3, modif3 = aux t3 in
                            (mk_triop ITE res1 res2 res3, (modif1 || modif2 || modif3))
                    end

            | Triop(Div, t1, t2, t3) -> 
                    begin match t1.node, t2.node, t3.node with
                    | Const c1, Const c2, Const c3 -> 
                            (mk_const (div c1 c2 c3), true)
                    | _, _, Const c when Bitvector.is_zeros c.value -> (mk_const (O.const_of_bitv (Bitvector.max_ubv (Bitvector.size_of c.value))), true)
                    | _, _, Const c when Bitvector.is_ones c.value -> 
                            (t2, true) (* only t2 because we reduce to size bits*)
                    | _, _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            let res3, modif3 = aux t3 in
                            (mk_triop Div res1 res2 res3, (modif1 || modif2 || modif3))
                    end

            | Triop(SDiv, t1, t2, t3) -> 
                    begin match t1.node, t2.node, t3.node with
                    | Const c1, Const c2, Const c3 -> 
                            (mk_const (sdiv c1 c2 c3), true)
                    | _, _, Const c when Bitvector.is_ones c.value -> 
                            (t2, true) (* only t2 because we reduce to size bits*)
                    | _, _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            let res3, modif3 = aux t3 in
                            (mk_triop SDiv res1 res2 res3, (modif1 || modif2 || modif3))
                    end

            | Triop(Mod, t1, t2, t3) ->
                    begin match t1.node, t2.node, t3.node with
                    | Const c1, Const c2, Const c3 ->
                            (mk_const (umod c1 c2 c3), true)
                    | _, _, Const c when Bitvector.is_ones c.value -> (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | _, _, _ when (tree_equal t1 t2) && (tree_equal t1 t3) ->
                            (mk_const (O.const_of_int 0 (t1.bitsz)), true)
                    | _, _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            let res3, modif3 = aux t3 in
                            (mk_triop Mod res1 res2 res3, (modif1 || modif2 || modif3))
                    end
            | Triop(SMod, t1, t2, t3) ->
                    begin match t1.node, t2.node, t3.node with
                    | Const c1, Const c2, Const c3 ->
                            (mk_const (smod c1 c2 c3), true)
                    | _, _, Const c when Bitvector.is_ones c.value -> (mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true)
                    | _, _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            let res3, modif3 = aux t3 in
                            (mk_triop SMod res1 res2 res3, (modif1 || modif2 || modif3))
                    end
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
                            | _ , _ ->
                                    let res1, modif1 = aux t1 in
                                    let res2, modif2 = aux t2 in
                                    (mk_binop Add res1 res2, (modif1 || modif2))
                            end

                    | Binop(Sub, t1', t2'), Const c
                    | Const c, Binop(Sub, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sum (sub c1 c2) c), true)
                            | _ , Const c1 -> (mk_binop Add t1' (mk_const (sub c c1)), true)
                            | Const c1, _ -> (mk_binop Sub (mk_const (sum c c1)) t2', true)
                            | _ , _ -> 
                                    let res1, modif1 = aux t1 in
                                    let res2, modif2 = aux t2 in
                                    (mk_binop Add res1 res2, (modif1 || modif2))
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
                    | _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop Add res1 res2, (modif1 || modif2))
                    end
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
                            | _ , _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Mul res1 res2, (modif1 || modif2))
                            end
                    | Unop(Minus, t1'), Unop(Minus, t2') -> (mk_binop Mul t1' t2', true)
                    | Unop(Minus, t1'), _ -> (mk_unop Minus (mk_binop Mul t1' t2), true)
                    | _, Unop(Minus, t2') -> (mk_unop Minus (mk_binop Mul t1 t2'), true)
                    | _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop Mul res1 res2, (modif1 || modif2))
                    end
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
                            | _ , _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Sub res1 res2, (modif1 || modif2))
                            end
                    | Const c, Binop(Sub, t1', t2') ->
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2 -> (mk_const (sub c (sub c1 c2)), true)
                            | _ , Const c1 -> (mk_binop Sub (mk_const (sum c c1)) t1', true)
                            | Const c1, _ -> (mk_binop Add (mk_const (sub c c1)) t2', true)
                            | _ , _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Sub res1 res2, (modif1 || modif2))
                            end
                    | Const c, Binop(Add, t1', t2') -> 
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2  -> (mk_const (sub c (sum c1 c2)), true) 
                            | Const c1, _ -> (mk_binop Sub (mk_const (sub c c1)) t2', true)
                            | _, Const c2 -> (mk_binop Sub (mk_const (sub c c2)) t1', true)
                            | _, _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Sub res1 res2, (modif1 || modif2))
                            end
                    | Binop(Add, t1', t2'), Const c -> 
                            begin match t1'.node, t2'.node with
                            | Const c1, Const c2  -> (mk_const (sub (sum c1 c2) c), true) 
                            | Const c1, _ -> (mk_binop Sub t2' (mk_const (sub c c1)), true)
                            | _, Const c2 -> (mk_binop Sub t1' (mk_const (sub c c2)), true)
                            | _, _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Sub res1 res2, (modif1 || modif2))
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
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop Sub res1 res2, (modif1 || modif2))
                    end

            | Binop(LShift, t1, t2) ->
                    begin match t1.node, t2.node with
                    | Const c1, Const c2 -> 
                            (mk_const (lshift c1 c2), true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | _, _ ->
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop LShift res1 res2, (modif1 || modif2))
                    end

            | Binop(RShiftu, t1, t2) ->
                    begin match t1.node, t2.node with
                    | Const c1, Const c2 -> 
                            (mk_const (rlshiftu c1 c2), true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | _, _ ->
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop RShiftu res1 res2, (modif1 || modif2))
                    end

            | Binop(RShifts, t1, t2) ->
                    begin match t1.node, t2.node with
                    | Const c1, Const c2 -> 
                            (mk_const (rlshifts c1 c2), true)
                    | _, Const c when Bitvector.is_zeros c.value ->
                            (t1, true)
                    | _, _ ->
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop RShifts res1 res2, (modif1 || modif2))
                    end

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
                            | _, _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
                            end
                    | Const c, Binop(Mul, t21', t22') when Bitvector.is_ones c.value ->
                            begin match t21'.node, t22'.node with
                            | Const c, _ when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, Const c when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, _ ->
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
                            end
                    | Binop(Mul, t11', t12'), Const c when Bitvector.is_ones c.value ->
                            begin match t11'.node, t12'.node with
                            | Const c, _ when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, Const c when not (Bitvector.get_bit c.value 0) -> (* if const is even *) 
                                    mk_const (O.const_of_int 0 (Bitvector.size_of c.value)), true
                            | _, _ ->
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
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
                            | _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
                            end
                    | _ , Unop(Minus, t') ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value -> (t1, true)
                            | _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
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
                            | _, _ ->
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop And res1 res2, (modif1 || modif2))
                            end
                    | _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop And res1 res2, (modif1 || modif2))
                    end

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
                            | _, _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Or res1 res2, (modif1 || modif2))
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
                            | _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Or res1 res2, (modif1 || modif2))
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
                    | _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop Or res1 res2, (modif1 || modif2))
                    end

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
                                    | _, _ -> 
                                        let res1, modif1 = aux t1 in
                                        let res2, modif2 = aux t2 in
                                        (mk_binop Xor res1 res2, (modif1 || modif2))
                                    end

                            | _, Const c, Binop(Or, ch1, ch2) when Bitvector.get_bit c.value 0 -> 
                                    begin match ch1.node, ch2.node with
                                    | _, Const c' when Bitvector.get_bit c'.value 0 && (tree_equal ch1 t11') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | Const c', _ when Bitvector.get_bit c'.value 0 && (tree_equal ch2 t11') -> 
                                            (mk_unop Minus (mk_const (O.const_of_int 2 (Bitvector.size_of c'.value))), true)
                                    | _, _ -> 
                                        let res1, modif1 = aux t1 in
                                        let res2, modif2 = aux t2 in
                                        (mk_binop Xor res1 res2, (modif1 || modif2))
                                    end
                            | _, _, _ -> 
                                    let res1, modif1 = aux t1 in 
                                    let res2, modif2 = aux t2 in 
                                    (mk_binop Xor res1 res2, (modif1 || modif2))
                            end

                    | _ , Unop(Minus, t') ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value ->
                                (mk_unop Not t1, true)
                            | _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Xor res1 res2, (modif1 || modif2))
                            end
                    | Unop(Minus, t'), _  ->
                            begin match t'.node with
                            | Const c when Bitvector.is_ones c.value ->
                                (mk_unop Not t2, true)
                            | _ -> 
                                let res1, modif1 = aux t1 in
                                let res2, modif2 = aux t2 in
                                (mk_binop Xor res1 res2, (modif1 || modif2))
                            end
                    | _, _ -> 
                            let res1, modif1 = aux t1 in
                            let res2, modif2 = aux t2 in
                            (mk_binop Xor res1 res2, (modif1 || modif2))
                    end
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
                            | _, _ ->
                                    let res1, modif1 = aux t1 in
                                    mk_unop Minus res1, modif1
                            end
                    | _ -> 
                        let res1, modif1 = aux t1 in
                        (mk_unop Minus res1, modif1)
                    end
            | Unop(Not, t1) -> 
                    begin match t1.node with
                    | Const c -> (mk_const (b_not c), true)
                    | Unop(Not, t1') -> (t1', true)
                    | _ -> 
                        let res1, modif1 = aux t1 in
                        (mk_unop Not res1, modif1)
                    end
            | _ -> (t, false)
            end
        in
        begin match (aux tree) with
        | tree, true -> simplify tree
        | tree, false -> tree
        end

        
end
