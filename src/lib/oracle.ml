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

open Yojson.Basic.Util

type variable = Tree.variable
type constant = Tree.constant

type inputs = (variable * Bitvector.t) array

type sample = {
    vars : inputs;
    res  : variable * Bitvector.t         ;
}

type t = {
    ctx     : variable array * variable ;
    consts  : constant array ;
    mutable samples : sample array ;
    mutable affineplus_samples : sample array ;
    mutable affinexor_samples : sample array ;
    ops : string list option ;
    orig_size : int option;
}

(** Oracle **)
module type ORACLE = sig
    val nvars : unit -> int
    val nconsts : unit -> int
    val nsamples : unit -> int
    val var_values : variable -> Bitvector.t array
    val const_values : constant -> Bitvector.t array
    val out_values : unit -> Bitvector.t array
    val out_var : unit -> variable
    val print : unit -> unit
    val vars : unit -> variable array
    val consts : unit -> constant array
    val get_sample : int -> sample
    val get_samples : unit -> sample array
    val get_affineplus_sample : int -> sample
    val get_affineplus_samples : unit -> sample array
    val get_affinexor_samples : unit -> sample array
    val naffineplussamples : unit -> int
    val get_affinexor_sample : int -> sample
    val naffinexorsamples : unit -> int
    val sample_output : sample -> variable * Bitvector.t
    val sample_inputs : sample -> (variable * Bitvector.t) array
    val add_sample : (variable * Bitvector.t) array -> (variable * Bitvector.t) -> unit 
    val get_expr_size : unit -> int option
end

let vars { ctx ; _ } = fst ctx

let consts { consts ; _ } = consts

let nvars t = Array.length (fst t.ctx)

let nconsts t = Array.length (t.consts)

let nsamples { samples ; _ } = Array.length samples
let naffineplussamples { affineplus_samples ; _ } = Array.length affineplus_samples
let naffinexorsamples { affinexor_samples ; _ } = Array.length affinexor_samples


let output_of_sample sample = sample.res
let inputs_of_sample sample = sample.vars


let var_values t (var : variable) =
    let rec aux vars name n i =
        if i = n then invalid_arg "Variable is not present in the oracle"
        else
            let ((var : variable), vl) = vars.(i) in
            if var.name = name then vl else aux vars name n (i + 1)
    in
    Array.map (fun { vars ; _ } -> aux vars var.name (Array.length vars) 0) t.samples

let const_values t (c : constant) =
    Array.make (nsamples t) c.value

let out_values { samples ; _ } = Array.map (fun { res ; _ } -> snd res) samples

let out_var { ctx = (_, var) ; _ } = var

let hex_to_int json = int_of_string (to_string json)
let to_string json = String.lowercase_ascii (to_string json)

(** Extract sampled inputs from json file **)
let get_sample_input json =
    json |> to_assoc
    |> List.map
        (fun (_, t) ->
            let var = Tree.var_of 
                (to_string (member "location" t)) 
                (hex_to_int (member "size" t)) 
            in
            let value = Bitvector.create
                            (Z.of_string (to_string (member "value" t)))
                            var.sz
            in
            (var, value))
    |> Array.of_list

(** Extract observed outputs from json file **)
let get_sample_output json =
    let json = member "0" json in
    let var = Tree.var_of 
        (to_string (member "location" json)) 
        (hex_to_int (member "size" json)) 
    in
    let hex_val = to_string (member "value" json) in
    (var, Bitvector.create (Z.of_string hex_val) var.sz)

(** Extract sampled inputs and observed outputs from json file **)
let get_sample json = {
    vars = get_sample_input  (member "inputs" json)  ;
    res  = get_sample_output (member "outputs" json) ;
}

(** Extract all samples from json file **)
let get_samplings json =
    json |> member "sampling" |> to_assoc
    |> (List.map (fun (_, t) -> get_sample t))
    |> Array.of_list

(** Extract all affine samples from json file **)
let get_affineplus_samplings json =
  let affineplus = json |> member "affineplus" in
  match affineplus with 
  | `Null -> 
    [||]  (* Return empty array *)
  | _ ->
    affineplus |> to_assoc
    |> List.map (fun (_, t) -> get_sample t)
    |> Array.of_list

let get_affinexor_samplings json =
  let affinexor = json |> member "affinexor" in
  match affinexor with 
  | `Null -> 
    [||]  (* Return empty array *)
  | _ ->
    affinexor |> to_assoc
    |> List.map (fun (_, t) -> get_sample t)
    |> Array.of_list


(** Extract inputs meta information from json file **)
let get_meta_inputs json =
    json |> member "initial" |> member "inputs" |> to_assoc
    |> List.map
        (fun (_, t) -> Tree.var_of
            (to_string (member "location" t))
            (hex_to_int (member "size" t)))
    |> Array.of_list

(** Extract outputs meta information from json file **)
let get_meta_output json =
    let json = json |> member "initial" |> member "outputs" |> member "0" in
    let name = to_string (member "location" json) in
    let size = hex_to_int (member "size" json) in
    Tree.var_of name size


(** Extract inputs and outputs meta information from json file **)
let get_meta json =
    (get_meta_inputs json, get_meta_output json)

let get_ops json =
    try
        Some (List.map (fun x -> to_string x) (to_list (member "ops" json)))
    with Type_error (_, _) -> None

let get_size json =
    hex_to_int (json |> member "initial" |> member "outputs" |> member "0" |> member "size")

let get_expr_size json =
    match json |> member "info" with
    | `Null -> -1
    | m -> to_int (member "exprsize" m)

(** Create constant values between min and max **)
let mk_consts min max sz =
    Array.init (1 + max - min) (fun i ->
        Tree.const_of_int (min + i) sz)

(** Pretty printer **)
let print t =
    let printf = Printf.printf in
    let print_context { ctx = (in_vars, out_var) ; _ } =
        printf "Variables:\n";
        Array.iter (fun (v : variable) -> printf "\t%s, %d\n" v.name v.sz) in_vars;
        printf "\n\t%s, %d\n" out_var.name out_var.sz
    in
    let print_sampling t =
        printf "Samples:\n";
        Array.iter (fun { vars ; res } ->
            Array.iter
                (fun ((v : variable), vl) ->
                    printf "\t%s: %s\n" v.name (Bitvector.to_hexstring vl)) vars;
                printf "\tResult: %s\n\n" (Bitvector.to_hexstring (snd res))) t.samples
    in
    print_context t; print_sampling t

let add_sample oracle inputs output = 
    let newsample = { vars=inputs; res=output} in
    oracle.samples <- Array.append oracle.samples [|newsample|]

let gen_oracle (t : t) : (module ORACLE) = 
    (module struct
        let oracle = t
        let nvars () = nvars oracle
        let nconsts () = nconsts oracle
        let nsamples () = nsamples oracle
        let var_values var = var_values oracle var
        let const_values const = const_values oracle const
        let out_values () = out_values oracle
        let out_var () = out_var oracle
        let print () = print oracle
        let vars () = vars oracle
        let consts () = consts oracle
        let get_sample i = oracle.samples.(i)
        let get_samples () = oracle.samples
        let get_affineplus_sample i = oracle.affineplus_samples.(i)
        let get_affineplus_samples () = oracle.affineplus_samples
        let get_affinexor_samples () = oracle.affinexor_samples
        let naffineplussamples () = naffineplussamples oracle
        let get_affinexor_sample i = oracle.affinexor_samples.(i)
        let naffinexorsamples () = naffinexorsamples oracle
        let sample_inputs = inputs_of_sample
        let sample_output = output_of_sample
        let add_sample = add_sample oracle
        let get_expr_size () = oracle.orig_size 
    end : ORACLE)

let print_samples (samples : sample array) : string = 
    Array.fold_left (fun acc s ->
        let str_inputs = Array.fold_left (fun acc i -> 
            let (i_var : variable), i_val = i in
            acc ^ i_var.name ^ ":" ^ (Bitvector.to_hexstring i_val) ^ " "
        ) "" s.vars in
        let res_var, res_val = s.res in
        let str_out = res_var.name ^ ":" ^ (Bitvector.to_hexstring res_val) in
        acc ^ str_inputs ^ "-> " ^ str_out ^ "\n"
    ) "" samples 

(** Create oracle from json **)
let of_json json (cst: int array): (module ORACLE) =
    let sz = get_size json in
    let newcst = Array.map (fun v -> Tree.const_of_int v sz) cst in
    let oracle = {
        ctx = get_meta json ;
        consts = Array.append (mk_consts 1 1 sz) newcst ;
        samples = get_samplings json ;
        affineplus_samples = get_affineplus_samplings json ;
        affinexor_samples = get_affinexor_samplings json ;
        ops = get_ops json ;
        orig_size = Some (get_expr_size json);
    } in
    gen_oracle oracle

(** Create oracle from json file **)
let of_json_file ~filename (cst: int array): (module ORACLE) =
    let json = Yojson.Basic.from_file filename in
    let sz = get_size json in
    let newcst = Array.map (fun v -> Tree.const_of_int v sz) cst in
    let oracle = {
        ctx = get_meta json ;
        consts = Array.append (mk_consts 1 1 sz) newcst ;
        samples = get_samplings json ;
        affineplus_samples = get_affineplus_samplings json ;
        affinexor_samples = get_affinexor_samplings json ;
        ops = get_ops json ;
        orig_size = Some (get_expr_size json);
    } in
    gen_oracle oracle

let create_ctx samples = 
    Array.map (fun v -> fst v) samples.(0).vars, fst samples.(0).res

let of_samples (samples: sample array) ?(affineplus_samples=[||]) ?(affinexor_samples=[||]) (cst: int array) : (module ORACLE) =
    let sz = Bitvector.size_of (snd samples.(0).res) in
    let newcst = Array.map (fun v -> Tree.const_of_int v sz) cst in
    let ctx = create_ctx samples in
    
    let oracle = {
        ctx = ctx;
        consts = Array.append (mk_consts 1 1 sz) newcst;
        samples = samples;
        affineplus_samples = affineplus_samples;
        affinexor_samples = affinexor_samples;
        ops = None;
        orig_size = None;
    } in
    gen_oracle oracle
 
let rec eval (t : unit Tree.t) inputs = 
    let module SizeMod = Operators.SizeMod in
    let module Unop = Operators.Unop in
    let module Binop = Operators.Binop in
    let module Triop = Operators.Triop in    
    match t.node with
    | Const c -> Array.init (Array.length inputs) (fun _ -> c.value)
    | Var v -> Array.map (fun i -> 
        let value_opt = Array.find_opt (fun ((var : variable), _) -> String.equal v.name var.name) i in
        let _, value = Option.get value_opt in
        value) inputs
    | SizeMod (op, t) -> Array.map (fun v -> SizeMod.apply op v) (eval t inputs)
    | Unop (op, t) -> Array.map (fun v -> Unop.apply op v) (eval t inputs)
    | Binop (op, t1, t2) -> Array.map2 (fun v1 v2 -> Binop.apply op v1 v2) (eval t1 inputs) (eval t2 inputs)
    | Triop (op, t1, t2, t3) -> Utility.map3 (fun v1 v2 v3 -> Triop.apply op v1 v2 v3) (eval t1 inputs) (eval t2 inputs) (eval t3 inputs)


let of_expr (t : unit Tree.t) (inputs : inputs array) ?(affineplus_inputs=[||]) ?(affinexor_inputs=[||]) (outvar : variable) (cst: int array) = 
    
    let outputs = eval t inputs in
    let samples : sample array = Array.mapi (fun id i -> {
        vars = i;
        res = (outvar, outputs.(id))
    }) inputs in

    let affineplus_out = eval t affineplus_inputs in
    let affineplus_samples : sample array = Array.mapi (fun id i -> {
        vars = i;
        res = (outvar, affineplus_out.(id))
    }) affineplus_inputs in

    let affinexor_out = eval t affinexor_inputs in
    let affinexor_samples : sample array = Array.mapi (fun id i -> {
        vars = i;
        res = (outvar, affinexor_out.(id))
    }) affinexor_inputs in

    let sz = Bitvector.size_of (snd samples.(0).res) in
    let newcst = Array.map (fun v -> Tree.const_of_int v sz) cst in
    let ctx = create_ctx samples in

    let oracle = {
        ctx = ctx;
        consts = Array.append (mk_consts 1 1 sz) newcst;
        samples = samples;
        affineplus_samples = affineplus_samples;
        affinexor_samples = affinexor_samples;
        ops = None;
        orig_size = Some t.sz;
    } in
    gen_oracle oracle
    

let dummyOracle =
    let oracle = {
        ctx = Tree.([|
            (var_of "v0" 32); (var_of "v1" 32);
            (var_of "v2" 32); (var_of "v3" 32);
            (var_of "v4" 32); (var_of "v5" 32);
        |], 
        (var_of "output" 32)) ;
        consts = [||] ;
        samples = [||] ;
        affineplus_samples = [||] ;
        affinexor_samples = [||];
        ops = None ;
        orig_size= Some 32;
    } in
    gen_oracle oracle