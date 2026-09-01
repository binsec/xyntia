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

open Binsec_sse.Types
module Sexpr = Binsec_symbolic.Default
module BvTbl = Binsec_symbolic.Default.Expr.Tbl
module XG = Xgrammar

module IntMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type Ir.builtin += 
    | BuiltinSample of int * (string * Dba.LValue.t) list


module BVInterval = Interval.BitVec
type perm = { mutable readable: BVInterval.t ; mutable writable: BVInterval.t }


module type CONTEXTGETTER = sig
  type state
  val symb : state -> Sexpr.Expr.t Dba_types.Var.Map.t
  val mem : state -> Sexpr.Memory.t
  val toformula : state -> Binsec_smtlib.Formula.formula
end

let json_samples : Yojson.Basic.t list ref = ref []
let out_formula = ref None

module XyntiaSamplerUtils (Engine : ENGINE with type Path.value = Sexpr.Expr.t) (ContextGetter : CONTEXTGETTER with type state = Engine.Path.state) 
    = struct
    type path = Engine.Path.t

    let resout = ref (match Xcli.OutDir.get_opt ()  with 
    | Some "stdout" -> XG.Stdout
    | Some dir -> 
          ignore (if Sys.file_exists dir then failwith "The output directory already exists"
                  else Sys.mkdir dir 0o777);
          XG.Dir dir  
    | None -> XG.Nothing)

    let npaths = ref 0
    let prune_cst_outputs = ref false
    let already_sampled = ref false

    type namemap = string BvTbl.t

    let names : namemap = BvTbl.create 0
    let nmem_var = ref 0
    let nmem_out = ref 0
    let optimal_sampling = ref false

    type input_info = Sexpr.Expr.t * (Bitvector.t option)

    (* id uniquely representing an output, name, expr, assigned value, inputs dependencies, size of the expression *)
    type output_info = int * string * Sexpr.Expr.t * (Bitvector.t option) * input_info list * int option
    
    (* Hashmap from output id to list of output info *)
    type samples = (int, output_info list) Hashtbl.t

    let sampling : samples = Hashtbl.create 0
    let sampling_affine_plus : samples = Hashtbl.create 0
    let sampling_affine_xor : samples = Hashtbl.create 0

    let domains : (string, int * int) Hashtbl.t = Hashtbl.create 0 
    let domain_for_all = ref None

    let instruction_callback inst env =
      match inst with
      | XG.Sample (nsamples, outs) -> [ Ir.Builtin (BuiltinSample (nsamples, 
        (List.map (function 
        | XG.Reg ((Var (name, _), _) as loc) -> name, Script.eval_loc loc env
        | _ -> assert false) outs))) ]
      | _ -> []

    let declaration_callback decl (_env : Script.env) _path = 
      match decl with
      | XG.Domain ("all", min, max) -> 
              domain_for_all := Some (min, max);
              true
      | XG.Domain (varname, min, max) -> 
              Hashtbl.add domains varname (min, max);
              true
      | XG.OptimalSampling -> 
              optimal_sampling := true;
              true
      | XG.PruneCstOut ->
              prune_cst_outputs := true;
              true
      | _ -> false

    let add_exprsize (id, name, expr, bv, inputs, esize) : output_info =
      let _ = assert (Option.is_none esize) in
      let seen = Hashtbl.create 0 in
      let rec aux e = 
        match Hashtbl.find_opt seen e with
        | Some sz -> sz
        | None -> (
          let sz = (
            match e with
            | Sexpr.Expr.Unary r -> 
                    let subexpr = r.x in
                    1 + aux subexpr
            | Sexpr.Expr.Binary r -> 
                    let subexpr1 = r.x in
                    let subexpr2 = r.y in
                    1 + aux subexpr2 + aux subexpr1
            | Sexpr.Expr.Ite r -> 
                    let subexpr1 = r.c in
                    let subexpr2 = r.t in
                    let subexpr3 = r.e in
                    1 + aux subexpr3 + aux subexpr2 + aux subexpr1
            | Sexpr.Expr.Var _ 
            | Sexpr.Expr.Load _
            | Sexpr.Expr.Cst _ -> 1) in
          Hashtbl.add seen e sz;
          sz
        )
      in
      (id, name, expr, bv, inputs, Some (aux expr))

    let add_inputs (id, name, expr, bv, inputs, esize) : output_info =
      let _ =  assert (List.length inputs = 0) in
      let seen = Hashtbl.create 0 in
      let rec aux e acc = 
        if Hashtbl.mem seen e then acc
        else (
          let acc = (match e with
          | Sexpr.Expr.Unary r -> 
                  let subexpr = r.x in
                  aux subexpr acc
          | Sexpr.Expr.Binary r -> 
                  let subexpr1 = r.x in
                  let subexpr2 = r.y in
                  aux subexpr2 (aux subexpr1 acc)
          | Sexpr.Expr.Ite r -> 
                  let subexpr1 = r.c in
                  let subexpr2 = r.t in
                  let subexpr3 = r.e in
                  aux subexpr3 (aux subexpr2 (aux subexpr1 acc))
          | Sexpr.Expr.Var {name; label; size; _ } as v -> 
                  let ({info; _ } : Dba.Var.t) = Dba.Var.create label ~bitsize:(Size.Bit.create size) ~tag:(Empty) in
                  begin match info with
                  | Flag -> acc (* prune flags from the set of inputs *)
                  | _ ->
                      if not (BvTbl.mem names v) then
                          BvTbl.add names v name;
                      if not (List.exists (fun (x, _) -> Sexpr.Expr.is_equal x v) acc) then
                          (v, None)::acc
                      else
                          acc
                  end
          | Sexpr.Expr.Load _ as l -> 
                  if not (BvTbl.mem names l) then begin 
                      let mname = Printf.sprintf "mem_%d" !nmem_var in
                      nmem_var := !nmem_var + 1;
                      BvTbl.add names l mname; 
                  end;
                  if not (List.exists (fun (x, _) -> Sexpr.Expr.is_equal x l) acc) then
                      (l, None)::acc
                  else
                      acc
          | Sexpr.Expr.Cst _ -> acc) in
          Hashtbl.add seen e ();
          acc
        )
      in
      (id, name, expr, bv, aux expr [], esize)

    let fixed_samples sampleid size = match sampleid with
    | 0 -> Bitvector.zeros size |> Option.some
    | 1 -> Bitvector.zeros size |> Bitvector.succ |> Option.some
    | 2 -> Bitvector.max_ubv size |> Option.some
    | 3 -> Bitvector.max_sbv size |> Option.some
    | 4 -> Bitvector.min_sbv size |> Option.some
    | _ -> None 

    let optimal_sample nsamples sampleid ratio size =
      if not !optimal_sampling then None
      else
          let nfix = 5 in
          if sampleid < nfix then 
              let fixed = fixed_samples sampleid size in
              if Option.is_none fixed then failwith "Discrepency: nb fixed samples < nfix - 1";
              fixed
          else (
              if fixed_samples sampleid size |> Option.is_none |> not then failwith "Discrepency between fixed samples matches and nfix";
              let nleft = nsamples - nfix in 
              let n = ratio *. (Float.of_int nleft) /. 100. |> Float.to_int in
              if sampleid - nfix < n then
                  Bitvector.rand size |> Option.some
              else
                  None
          )
    (* Get id of current variable + id of sample to return appropriate BV*)
    let affine_sample varid sampleid size const =                    
    if sampleid = (varid + 1) then
      Bitvector.of_int ~size:size const |> Option.some
    else 
      Bitvector.zeros size |> Option.some
  
    (* counter for getting the right index *)
    let current_symb_var = ref 0
    let num_var = ref 0
  
    let affine_symb const _nsamples sampleid (expr : Sexpr.Expr.t) =
      let size = Sexpr.Expr.sizeof expr in
      let varid = !current_symb_var in
      let res = match affine_sample varid sampleid size const with
      | Some v -> v
      | None -> failwith "affine_sample returned None"
      in
      if !num_var <> 0 then
        begin
        current_symb_var := (!current_symb_var + 1 )mod !num_var;
        end
      else 
        failwith "no symb vars";
      res
    

    let affineplus_symb nsamples sampleid expr = 
      affine_symb 1 nsamples sampleid expr
    let affinexor_symb nsamples sampleid expr = 
      affine_symb (-1) nsamples sampleid expr

    let random_symb nsamples sampleid (expr : Sexpr.Expr.t) = 
      let size = Sexpr.Expr.sizeof expr in
      match optimal_sample nsamples sampleid 10.0 size with
      | Some v -> v
      | None -> begin match expr with
          | Var { label; _ } when Hashtbl.mem domains label -> 
              let min, max = Hashtbl.find domains label in
              let bvmin, bvmax = Bitvector.of_int ~size min, Bitvector.of_int ~size max in
              let r = Bitvector.rand size in
              Bitvector.umod r ((Bitvector.sub bvmax bvmin) |> Bitvector.succ) |> Bitvector.add bvmin
          | _ -> 
              begin match !domain_for_all with 
              | Some (min, max) ->
                let bvmin, bvmax = Bitvector.of_int ~size min, Bitvector.of_int ~size max in
                let r = Bitvector.rand size in
                Bitvector.umod r ((Bitvector.sub bvmax bvmin) |> Bitvector.succ) |> Bitvector.add bvmin
              | None -> Bitvector.rand size
              end
      end

    (* counter for getting the right index *)
    let current_mem_var = ref 0
    
    let affine_mem const _nsamples sampleid _mem _addr size (_endianess: Term.endianness) =
      let size = 8*size in
      let varid = !current_mem_var in
      let res = match affine_sample varid sampleid size const with
      | Some v -> v
      | None -> failwith "affine_sample returned None"
      in
      if !num_var <> 0 then
        begin
        current_mem_var := (!current_mem_var + 1) mod !num_var;
        end
      else 
        failwith "no mem vars";
      res

    let affineplus_mem nsamples sampleid mem addr size (endianess: Term.endianness)= 
      affine_mem 1 nsamples sampleid mem addr size (endianess: Term.endianness)
    let affinexor_mem nsamples sampleid mem addr size (endianess: Term.endianness) = 
      affine_mem (-1) nsamples sampleid mem addr size (endianess: Term.endianness)

    let random_mem nsamples sampleid _mem _addr size (_endianess: Term.endianness) =
        let size = 8*size in
        match optimal_sample nsamples sampleid 10.0 size with
        | Some v -> v
        | None -> begin match !domain_for_all with 
          | Some (min, max) ->
              let bvmin, bvmax = Bitvector.of_int ~size min, Bitvector.of_int ~size max in
              let r = Bitvector.rand size in
              Bitvector.umod r ((Bitvector.sub bvmax bvmin) |> Bitvector.succ) |> Bitvector.add bvmin
          | None -> Bitvector.rand size
        end

    let evaluate_input nsamples sampleid model (expr, _) symb_strat mem_strat : input_info =
        let bv = Modeleval.eval ~symbols:(symb_strat nsamples sampleid) ~memory:(mem_strat nsamples sampleid) model expr in
        (expr, Some bv)
    
    let evaluate_output nsamples sampleid model (id, name, expr, _, inputs, esize) symb_strat mem_strat : output_info = 
        (* Get number of inputs *)
        num_var := List.length inputs;

        (* In theory by evaluating the output all the inputs are also set and so the model is already complete *)
        let bv = Modeleval.eval ~symbols:(symb_strat nsamples sampleid) ~memory:(mem_strat nsamples sampleid) model expr in 
        let inputs = List.fold_left (fun acc info -> (evaluate_input nsamples sampleid model info symb_strat mem_strat)::acc) [] inputs in 
        (id, name, expr, Some bv, inputs, esize)


    let get_regouts regmap acc = Dba_types.Var.Map.fold 
                  (fun key value acc -> ((List.length acc), key.name, value, None, [], None)::acc)
                  regmap acc
        

    let get_memouts lmap acc = 
        let rec aux (lmap : Sexpr.Memory.t) acc = match lmap with
        | Layer {over=next;addr=base;store; _} as layer -> 
                let acc = Sexpr.Store.fold_term 
                      (fun offset expr cumul -> 
                          let endianess = Machine.ISA.endianness (Engine.isa) in
                          let sizeofaddr = Machine.ISA.word_size (Engine.isa) in
                          let loadaddr = Sexpr.Expr.add base (Sexpr.Expr.constant (Bitvector.create offset sizeofaddr)) in
                          let bytesize = (Sexpr.Expr.sizeof expr) / 8 in
                          let loadexpr = Sexpr.Expr.load bytesize endianess loadaddr layer in
                          assert (not (BvTbl.mem names loadexpr));
                          let mname = Printf.sprintf "outmem_%d" !nmem_out in
                          nmem_out := !nmem_out + 1;
                          BvTbl.add names loadexpr mname; 
                          ((List.length cumul), mname, expr, None, [], None)::cumul
                      ) acc store in
                aux next acc
        | Symbol _ -> acc
        in
        aux lmap acc

    let tojson (outsinfos : output_info list) = 
        let samples infos = List.fold_left (fun acc info -> 
                  let _, oname, _, obv, inputs, _ = info in
                  let obv = Option.get obv in
                  let ohexsize = Printf.sprintf "0x%x" ((Bitvector.size_of obv)) in
                  let toadd = (Int.to_string (List.length acc), 
                                  `Assoc [ 
                                      ("inputs", `Assoc (List.fold_left (fun acc2 input -> 
                                          let iexpr, ibv = input in
                                          let iloc = BvTbl.find names iexpr in
                                          let iname = match iexpr with
                                          | Load {len; addr; _} -> Format.asprintf "[%a, %d]" Term.pp addr len
                                          | _ -> iloc
                                          in
                                          let ibv = Option.get ibv in
                                          let ihexsize = Printf.sprintf "0x%x" ((Bitvector.size_of ibv)) in
                                          (Int.to_string (List.length acc2), 
                                              `Assoc [ 
                                                  ("location", `String iloc); 
                                                  ("name", `String iname); 
                                                  ("size", `String ihexsize); 
                                                  ("value", `String (Bitvector.to_hexstring ibv)) 
                                              ] 
                                          )::acc2
                                      ) [] inputs));  
                                      ("outputs", 
                                          `Assoc [ ("0", `Assoc [ 
                                              ("location", `String oname); 
                                              ("name", `String oname); 
                                              ("size", `String ohexsize); 
                                              ("value", `String (Bitvector.to_hexstring obv)) 
                                          ] )]
                                      )
                                  ]
                  ) in 
                  toadd::acc
              ) [] infos 
        in
        let samp = samples outsinfos in
        let _, _, _expr, _, _, exprsize = List.hd outsinfos in 
        `Assoc [ 
            ("initial", snd (List.hd samp)); 
            ("sampling", `Assoc samp) ;
            (* ("info"), `Assoc [ ("exprsize", `Int (Option.get exprsize)); ("binsec_formula", `String (Format.asprintf "%a" Term.pp expr)) ] *)
            ("info"), `Assoc [ ("exprsize", `Int (Option.get exprsize)); ]
        ]

        let bv_to_smtstr bv = 
          let s = Bitvector.to_hexstring bv in
          String.mapi (fun i c -> if i = 0 then '#' else c) s

        let tosygus (outsinfos : output_info list) = 
          let varorder = Hashtbl.create 0 in
          let sizemapping = Hashtbl.create 0 in (*must be a better way to do it*)
          let outsize = ref 0 in
          let varid = ref 0 in
          let constrs_l = List.map (fun (oinfo : output_info) -> 
              let _, _ ,_ , out_opt, iinfo, _ = oinfo in

              (* Set variables order *)
              ignore (List.iter (fun (iexpr, ibv_opt) -> 
                let iloc = BvTbl.find names iexpr in
                if not (Hashtbl.mem sizemapping iloc) then (
                  Hashtbl.add sizemapping iloc (Bitvector.size_of (Option.get ibv_opt))
                );
                if not (Hashtbl.mem varorder iloc) then ( 
                  Hashtbl.add varorder iloc !varid;
                  varid := !varid + 1
                )
              ) iinfo);

              let out = Option.get out_opt in
              outsize := Bitvector.size_of out;

              (* get ordered vector of inputs *)
              let inputs = Array.init !varid (fun _ -> None) in
              ignore (List.iter (fun (iexpr, i) -> 
                  let iloc = BvTbl.find names iexpr in
                  let vid = Hashtbl.find varorder iloc in
                  Array.set inputs vid i) iinfo);
                
              
              let s = Array.fold_left (fun acc bv -> (acc ^ " ") ^ (bv_to_smtstr (Option.get bv))) String.empty inputs in
              Printf.sprintf "(constraint (= (f %s) %s))" s (bv_to_smtstr out)
            ) outsinfos 
          in
          let str_varorder = Hashtbl.fold (fun k v acc -> 
            let s = Printf.sprintf "v%d: %s<%d>" v k (Hashtbl.find sizemapping k) in
            s::acc
          ) varorder [] in
          let comment  = (Printf.sprintf ";out: out<%d>\n;" !outsize) ^ String.concat "\n; " str_varorder in
          comment ^ "\n" ^ (String.concat "\n" constrs_l)

    let sample_preconds state nsamples sampleid model symbfun memfun = 
      (* samples values used in preconditions 
         Returns true if the samples match the precondition and false otherwise
      *)
      let preconds = Engine.Path.State.predicate state in 
      List.for_all (fun p -> 
        let info : output_info = (Int.max_int, "", p, None, [], None) in
        let (_, _, _, value, _, _) = evaluate_output nsamples sampleid model info symbfun memfun in
        Bitvector.is_ones (Option.get value)
      ) preconds

    let sample_affine htbl symb_func mem_func outsinfo state =
      (* affine sampling*)                    
      let max_inputs_length lst =
        List.fold_left (fun acc (_, _, _, _, inputs, _) ->
          let len = List.length inputs in
          if len > acc then len else acc
        ) 0 lst
      in
      let num_input = max_inputs_length outsinfo in 
      for sampleid=0 to num_input do
          (* Evaluate inputs *)
          let model = Sexpr.Model.empty () in
          let evaluated_outsinfo = (if sample_preconds state 1 sampleid model symb_func mem_func then
            Some (List.fold_left (fun acc info -> (evaluate_output 1 sampleid model info symb_func mem_func)::acc) [] outsinfo)
          else 
            None)
          in
          begin match evaluated_outsinfo with 
          | Some e ->
              (* Update sampling affine *)
              List.iter (fun outinfo -> 
                      let id, _name, _expr, _, _, _ = outinfo in 
                      let old = Hashtbl.find_opt htbl id in
                      match old with
                      | Some l -> 
                        if not (List.mem outinfo l) then
                          Hashtbl.replace htbl id (outinfo :: l)
                      | None -> Hashtbl.add htbl id [outinfo]
                  ) e;
          | _ -> ()
          end
      done

    let find_index l f = 
    let l' = List.mapi (fun i e -> if f(e) then Some (i, e) else None) l in 
    let l'' = List.filter_map Fun.id l' in
    if List.length l'' <= 0 then 
      None
    else
      Some (fst (List.hd l''))
  
    type tofix_input = Symb of Sexpr.Expr.t | Mem of (Bitvector.t * int)
    
    let is_last_path () = 
      (Engine.Metrics.Exploration.Paths.get Pending) <= 0


    let nsuccess_sampling = ref 0
    let asked_nsamples = ref 0
    let samples_saved = ref false

    let sample nsamples (outs : (string * Dba.LValue.t) list) state : unit  =
            (* 
              * The get_vsymb function might return outputs that are in fact only read 
              * So we need to filter such useless outputs
              * rem: there is no such problem for get_vmem
              * *)
            let symbols =  Dba_types.Var.Map.filter (fun ({ name ; info; _ } : Dba.Var.t) (expr : Sexpr.Expr.t) ->
                info <> Temp && (* filter Binsec IR temporary variables *)
                info <> Flag && (* filter flags from the detected outputs *)
                match expr with (* filter outputs which are just reads *)
                | Var { label; _ } -> name <> label
                | _ -> true) (ContextGetter.symb state) 
            in

            (* Get all outputs *)
            let outsinfo = get_regouts symbols [] in
            let outsinfo = match ContextGetter.mem state with 
            | a -> get_memouts a outsinfo 
            | exception Not_found -> outsinfo 
            in

            let outsinfo = if List.length outs > 0 then ( 
                                (* keep only the outputs specified by the user (can be subregisters) *)
                                List.filter_map (fun (id, name, e, value, dep, size) -> 
                                try (
                                  let outname, outvar = List.find (fun (_outname, outvar) -> 
                                    match (outvar: Dba.LValue.t )with 
                                    | Var {name=name'; _} | Restrict ({name=name'; _}, _) -> String.equal name name'
                                    | _ -> false
                                  ) outs
                                  in 
                                  (match outvar with
                                  | Var _ -> Some (id, outname, e, value, dep, size)
                                  | Restrict (_, range) ->
                                    let e' = Sexpr.Expr.unary (Restrict range ) e in
                                    Some (id, outname, e', value, dep, size)
                                  | _ -> assert false)
                                )
                                with Not_found -> None) outsinfo)
                          else
                                outsinfo in
            
            (* Get expression size in term of number of operators, vars, load and constants *)
            let outsinfo = List.fold_left (fun acc info -> (add_exprsize info)::acc) [] outsinfo in
            (* Get all inputs *)
            let outsinfo = List.fold_left (fun acc info -> (add_inputs info)::acc) [] outsinfo in

            (* filter outputs with no inputs *)
            let outsinfo = List.filter (fun (_, _, _, _, inp, _) -> (not !prune_cst_outputs) || List.length inp > 0) outsinfo in

            let ntests = 10*nsamples in
            for sampleid=0 to ntests-1 do
              if !nsuccess_sampling < nsamples then (
                  (* Evaluate inputs *)
                  let model = Sexpr.Model.empty () in
                  let verifies_preconds = sample_preconds state nsamples sampleid model random_symb random_mem in 

                  let evaluated_outsinfo = if verifies_preconds then
                    Some (List.fold_left (fun acc info -> (evaluate_output nsamples sampleid model info random_symb random_mem)::acc) [] outsinfo)
                  else 
                    None
                  in

                  begin match evaluated_outsinfo with 
                  | Some e ->
                      Spinner.update (fun () -> nsuccess_sampling := !nsuccess_sampling + 1);
                      (* Update sampling *)
                      List.iter (fun outinfo -> 
                              let id, _name, _expr, _, _, _ = outinfo in 
                              let old = Hashtbl.find_opt sampling id in
                              match old with
                              | Some l -> Hashtbl.replace sampling id (outinfo::l)
                              | None -> Hashtbl.add sampling id [outinfo]
                          ) e
                  | None -> () 
                  end
              )
              else ()
            done;
            
            ignore (if !nsuccess_sampling != nsamples then (failwith "[Error] Cannot synthesize block"));

            (* Create variables in the state for each Load and output expressions *)
            let new_state = BvTbl.fold (fun expr name acc -> 
              begin match expr with 
              | Load _ -> 
                let bitsize = Size.Bit.create (Sexpr.Expr.sizeof expr) in
                Engine.Path.State.assign (Dba.Var.create name ~bitsize ~tag:Empty) expr acc
              | _ -> acc end) names state in

            let new_state = List.fold_left (fun acc (_, name, e, _, _, _) -> 
              let bitsize = Size.Bit.create (Sexpr.Expr.sizeof e) in
              Engine.Path.State.assign (Dba.Var.create name ~bitsize ~tag:Empty) e acc
            ) new_state outsinfo in

            out_formula := Some (ContextGetter.toformula new_state);

            ignore (match !resout with
            | Dir dir -> 
                  let ochan = open_out (String.concat "/" [ dir; "formula" ]) in
                  let fmt = (Format.formatter_of_out_channel ochan) in
                  Binsec_smtlib.Formula.pp_formula fmt (Option.get !out_formula); 
                  Format.pp_print_flush fmt ();
                  close_out ochan
            | _ -> ())

    let states = ref None
    let exploration_finished = ref false
    let end_process = ref false
    let glob_nsamples = ref None
    let glob_outs = ref None

    let sample_all_paths nsamples (outs : (string * Dba.LValue.t) list) path : unit  =
        glob_nsamples := Some nsamples;
        glob_outs := Some outs;
        let s = Engine.Path.state path in
        (states := match !states with 
        | Some s' -> Some (Engine.Path.State.merge s s')
        | None -> Some s)
    
    let builtin_callback = function
        | BuiltinSample (nsamples, outs) -> 
          asked_nsamples := nsamples;
          Apply (sample_all_paths nsamples outs)
        | _ -> Unknown
    

    let at_exit_callback () =  
      (match Xcli.Mode.get () with
      | Xyntia ->
        Hashtbl.iter (fun oid outsinfos -> 
                    (* Printf.printf "OID %d\n" oid; *)
                    let json = tojson outsinfos in
                    json_samples := json::!json_samples;
                    match !resout with
                    | Nothing -> ()
                    | Stdout -> Printf.printf "%s\n" (Yojson.pretty_to_string json)
                    | Dir dir -> 
                            let id, name, _, _, _, _ = List.hd outsinfos in
                            assert (oid = id);
                            let fname = Printf.sprintf "%s/%s.json" dir name in 
                            let file = open_out fname in
                            Printf.fprintf file "%s\n" (Yojson.pretty_to_string json);
                            flush file;
                            close_out file
                ) sampling
      | SyGus -> 
        Hashtbl.iter (fun oid outsinfos -> 
          let constrs = tosygus outsinfos in         
          match !resout with
          | Nothing -> ()
          | Stdout -> Printf.printf "%s\n" constrs
          | Dir dir -> 
                  let id, name, _, _, _, _ = List.hd outsinfos in
                  assert (oid = id);
                  let fname = Printf.sprintf "%s/%s.sl" dir name in 
                  let file = open_out fname in
                  Printf.fprintf file "%s\n" constrs;
                  flush file;
                  close_out file
                ) sampling);
      Spinner.update (fun () -> samples_saved := true; end_process := true);
      Spinner.stop ()

      let init_callback _ = 
         (if !npaths = 0 then
              Spinner.create (exploration_finished, npaths, nsuccess_sampling, asked_nsamples, samples_saved, end_process))

      let signal_callback _path (status : status) = match status with
      | Halt | Cut | Assertion_failure -> (
        (* If the last path enumerated halts then sample_all_paths will not be called anymore.
           In such a case, we need to call sample here. *)
        Spinner.update (fun () -> npaths := !npaths + 1);
        let n = Spinner.update (fun () -> !npaths) in 
        (if is_last_path () || n = Xcli.MaxPath.get () then (
          Spinner.update (fun () -> exploration_finished := true);
          (if (Option.is_none !glob_nsamples) || (Option.is_none !glob_outs) then (
            Format.print_string "No path reached the sampling command\n";
            raise Binsec_sse.Exec.Halt));
          sample (Option.get !glob_nsamples) (Option.get !glob_outs) (Option.get !states);
          raise Binsec_sse.Exec.Halt)))
      | Unsatisfiable_assumption -> (
        Format.print_string "Reach an unsatpath\n";
        raise Binsec_sse.Exec.Halt)
      | Enumeration_limit -> (
        Format.print_string "Reach enumeration limit\n";
        raise Binsec_sse.Exec.Halt)
      | Max_depth ->( 
        Format.print_string "Reach a max depth error during the Binsec sampling (use -sse-depth command to solve)\n";
        raise Binsec_sse.Exec.Halt)
      | _ -> (
        Format.print_string "Found a path not reaching the end of reverse windows\n";
        raise Binsec_sse.Exec.Halt)

end

