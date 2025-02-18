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

open Yojson.Basic.Util

type variable  = { name : string ; sz : int }
type constant = { name : string ; value : Bitvector.t }

type sample = {
    vars : (variable * Bitvector.t) array ;
    res  : variable * Bitvector.t         ;
}

type t = {
    ctx     : variable array * variable ;
    consts  : constant array ;
    mutable samples : sample array   ;
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
    val random_var : unit -> variable
    val random_const : unit -> constant
    val print : unit -> unit
    val vars : unit -> variable array
    val consts : unit -> constant array
    val ops : unit -> string list option
    val const_of_int : int -> int -> constant
    val const_of_bitv : Bitvector.t -> constant
    val get_sample : int -> sample
    val sample_output : sample -> variable * Bitvector.t
    val sample_inputs : sample -> (variable * Bitvector.t) array
    val add_sample : (variable * Bitvector.t) array -> (variable * Bitvector.t) -> unit 
    val get_expr_size : unit -> int option
end

let ops {ops ; _} = ops

let vars { ctx ; _ } = fst ctx

let consts { consts ; _ } = consts

let nvars t = Array.length (fst t.ctx)

let nconsts t = Array.length (t.consts)

let nsamples { samples ; _ } = Array.length samples

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

let const_values t c =
    Array.make (nsamples t) c.value

let out_values { samples ; _ } = Array.map (fun { res ; _ } -> snd res) samples

let out_var { ctx = (_, var) ; _ } = var

let const_of_int value sz = {name=(string_of_int value); value=(Bitvector.of_int ~size:sz value)}
let const_of_bitv value = { name=(Bitvector.to_string value); value=value}

(** Create a variable **)
let mk_var name sz =
    if sz <= 0 then invalid_arg "Variable size should be positive"
    else if String.length name = 0 then
        invalid_arg "Variable name should be non-empty";
    { name ; sz }

(** Get a random variable **)
let random_var t =
    let vars = fst t.ctx in
    vars.(Random.int (nvars t))

(** Get a random constant value **)
let random_const t =
    (t.consts).(Random.int (Array.length t.consts))

let hex_to_int json = int_of_string (to_string json)
let to_string json = String.lowercase_ascii (to_string json)

(** Extract sampled inputs from json file **)
let get_sample_input json =
    json |> to_assoc
    |> List.map
        (fun (_, t) ->
            let var = {
                name = to_string (member "location" t) ;
                sz =  hex_to_int (member "size" t);
            }
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
    let var = {
        name = to_string (member "location" json) ;
        sz   = hex_to_int (member "size" json);
    }
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

(** Extract inputs meta information from json file **)
let get_meta_inputs json =
    json |> member "initial" |> member "inputs" |> to_assoc
    |> List.map
        (fun (_, t) ->
            { name = to_string (member "location" t) ;
            sz = hex_to_int (member "size" t)})
    |> Array.of_list

(** Extract outputs meta information from json file **)
let get_meta_output json =
    let json = json |> member "initial" |> member "outputs" |> member "0" in
    let name = to_string (member "location" json) in
    let size = hex_to_int (member "size" json) in
    mk_var name size


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
        const_of_int (min + i) sz)

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
        let random_var () = random_var oracle
        let random_const () = random_const oracle
        let print () = print oracle
        let vars () = vars oracle
        let consts () = consts oracle
        let ops () = ops oracle
        let const_of_int = const_of_int
        let const_of_bitv = const_of_bitv
        let get_sample i = oracle.samples.(i)
        let sample_inputs = inputs_of_sample
        let sample_output = output_of_sample
        let add_sample = add_sample oracle
        let get_expr_size () = oracle.orig_size 
    end : ORACLE)

(** Create oracle from json file **)
let of_json ~filename (cst: int array): (module ORACLE) =
    let json = Yojson.Basic.from_file filename in
    let sz = get_size json in
    let newcst = Array.map (fun v -> {name=Int.to_string v; value=(Bitvector.of_int ~size:sz v)}) cst in
    let oracle = {
        ctx = get_meta json ;
        consts = Array.append (mk_consts 1 1 sz) newcst ;
        samples = get_samplings json ;
        ops = get_ops json           ;
        orig_size = Some (get_expr_size json);
    } in
    gen_oracle oracle
