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
    samples : sample array   ;
    ops : string list option ;
}


(** Strategy used to sample inputs **)
module type SamplingStrat = sig 
  val gen_random_int : unit -> int array array
end

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
end

let ops {ops ; _} = ops

let vars { ctx ; _ } = fst ctx

let consts { consts ; _ } = consts

let nvars t = Array.length (fst t.ctx)

let nconsts t = Array.length (t.consts)

let nsamples { samples ; _ } = Array.length samples

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

let const_of_int value sz = {name=(string_of_int value); value=(Bitvector.of_int value sz)}
let const_of_bitv value = { name=(Bitvector.print_dec value); value=value}

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

let to_int json = int_of_string (to_string json)
let to_string json = String.lowercase_ascii (to_string json)

(** Extract sampled inputs from json file **)
let get_sample_input json =
    json |> to_assoc
    |> List.map
        (fun (_, t) ->
            let var = {
                name = to_string (member "location" t) ;
                sz =  to_int (member "size" t) * 8 ;
            }
            in
            let value = Bitvector.extend
                            (Bitvector.of_hexstring (to_string (member "value" t)))
                            var.sz
            in
            (var, value))
    |> Array.of_list

(** Extract observed outputs from json file **)
let get_sample_output json =
    let json = member "0" json in
    let var = {
        name = to_string (member "location" json) ;
        sz   = to_int (member "size" json) * 8    ;
    }
    in
    let hex_val = to_string (member "value" json) in
    (var, Bitvector.extend (Bitvector.of_hexstring hex_val) var.sz)

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
            sz = to_int (member "size" t) * 8 })
    |> Array.of_list

(** Extract outputs meta information from json file **)
let get_meta_output json =
    let json = json |> member "initial" |> member "outputs" |> member "0" in
    let name = to_string (member "location" json) in
    let size = to_int (member "size" json) * 8 in
    mk_var name size


(** Extract inputs and outputs meta information from json file **)
let get_meta json =
    (get_meta_inputs json, get_meta_output json)

let get_ops json =
    try
        Some (List.map (fun x -> to_string x) (to_list (member "ops" json)))
    with Type_error (_, _) -> None

let get_size json =
    to_int (json |> member "initial" |> member "outputs" |> member "0" |> member "size") * 8


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

(** Create oracle from json file **)
let of_json ~filename : (module ORACLE) =
    let json = Yojson.Basic.from_file filename in
    (module struct
        let oracle = {
            ctx = get_meta json ;
            consts = mk_consts 1 1 (get_size json) ;
            samples = get_samplings json ;
            ops = get_ops json           ;
        }
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
    end : ORACLE)

(** Create oracle from an Ocaml function **)
let of_fun ~f:(f : int array -> int)  ~ninputs ~sampling_sz ~const_bounds gen_random_int : (module ORACLE) =
    let sz = 32 in
    let randomvals = gen_random_int () in
    let bitv_of_int x =
        Bitvector.of_int32 (Int32.of_int x)
    in
    let ovar = mk_var ("x_" ^ string_of_int ninputs) sz in
    let ctx =
        (Array.init ninputs
            (fun i -> mk_var ("x_" ^ string_of_int i) sz), ovar)
    in
    let samples =
        Array.init sampling_sz
            (fun i ->
                let values = randomvals.(i)
                in

                let vars =
                    Array.init ninputs (fun i -> ((fst ctx).(i), bitv_of_int values.(i)))
                in
                let res = (ovar, bitv_of_int (f values)) in
                (*let _ = print_samples values res in*)
                { vars ; res })
    in
    let min_const, max_const = const_bounds in
    (module struct
        let oracle = { ctx ; consts = mk_consts min_const max_const 32 ; samples ; ops = None}
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
     end : ORACLE)


(** Generate a random integer in [-bound; bound[  **)
let gen_tiny_int bound = 
    let lim_sup = bound in 
    let _ = Random.self_init () in
    if Random.bool () then
        Random.int lim_sup
    else
        -(Random.int lim_sup) - 1 

(** Generate a random integer in [-2^31; 2^31[ **)
let gen_full_int () =
    gen_tiny_int 0x80000000

(** Sample inputs with values in [-50; 50[ 
    * and special inputs i.e. only 0 or only 1 if limits is set **)
let tiny50 limits nargs nbio : (module SamplingStrat) = 
    if nargs < 0 then failwith "nargs must be >= 0";
    if nbio <= 0 then failwith "nb of I/O samples must be > 0";
    let li = match limits with
    | Some i ->
            if nbio < i then failwith "with limits option, nb of I/O samples must be >= nb of limits"
            else if i > 5 then failwith "nb of limits can't be greater than 5" 
            else i
    | None -> 0 
    in
    (module struct
        let gen_random_int () = 
            Array.init nbio (fun i -> 
                if i < (nbio - li) then 
                    (Array.init nargs (fun _ -> gen_tiny_int 50))
                else match (i - (nbio-li)) with
                | 0 -> Array.init nargs (fun _ -> 0)
                | 1 -> Array.init nargs (fun _ -> 1)
                | 2 -> Array.init nargs (fun _ -> -1)
                | 3 -> Array.init nargs (fun _ -> 0x7fffffff)
                | 4 -> Array.init nargs (fun _ -> 0x80000000)
                | _ -> assert false)
    end : SamplingStrat)


(** Sample inputs with values in [-1000; 1000[ 
    * and special inputs i.e. only 0 or only 1 if limits is set **)
let tiny1000 limits nargs nbio : (module SamplingStrat) = 
    if nargs < 0 then failwith "nargs must be >= 0";
    if nbio <= 0 then failwith "nb of I/O samples must be > 0";
    let li = match limits with
    | Some i ->
            if nbio < i then failwith "with limits option, nb of I/O samples must be >= nb of limits"
            else if i > 5 then failwith "nb of limits can't be greater than 5" 
            else i
    | None -> 0 
    in
    (module struct
        let gen_random_int () = 
            Array.init nbio (fun i -> 
                if i < (nbio - li) then 
                    (Array.init nargs (fun _ -> gen_tiny_int 1000))
                else match (i - (nbio-li)) with
                | 0 -> Array.init nargs (fun _ -> 0)
                | 1 -> Array.init nargs (fun _ -> 1)
                | 2 -> Array.init nargs (fun _ -> -1)
                | 3 -> Array.init nargs (fun _ -> 0x7fffffff)
                | 4 -> Array.init nargs (fun _ -> 0x80000000)
                | _ -> assert false)
    end : SamplingStrat)

(** Sample any 32bits values 
    * and special inputs i.e. only 0 or only 1 if limits is set **)
let full limits nargs nbio : (module SamplingStrat) = 
    if nargs < 0 then failwith "nargs must be >= 0";
    if nbio <= 0 then failwith "nb of I/O samples must be > 0";
    let li = match limits with
    | Some i ->
            if nbio < i then failwith "with limits option, nb of I/O samples must be >= nb of limits"
            else if i > 5 then failwith "nb of limits can't be greater than 5" 
            else i
    | None -> 0 
    in
    (module struct
        let gen_random_int () = 
            Array.init nbio (fun i -> 
                if i < (nbio - li) then 
                    (Array.init nargs (fun _ -> gen_full_int ()))
                else match (i - (nbio-li)) with
                | 0 -> Array.init nargs (fun _ -> 0)
                | 1 -> Array.init nargs (fun _ -> 1)
                | 2 -> Array.init nargs (fun _ -> -1)
                | 3 -> Array.init nargs (fun _ -> 0x7fffffff)
                | 4 -> Array.init nargs (fun _ -> 0x80000000)
                | _ -> assert false)
    end : SamplingStrat)

