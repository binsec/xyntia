open Types
(*open Yojson.Basic.Util*)

module IntMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type output = Reg of Ast.Loc.t Ast.loc
type resultout = Dir of string | Stdout 

type out_type = Xyntia | SyGus

type Ast.Obj.t += 
    | Out of output
    | OutList of output list
    | ResOut of resultout


type Ast.t += 
    | SetOutput of resultout
    | Domain of string * int * int
    | OptimalSampling
    | PruneCstOut 

type Ir.builtin += 
    | BuiltinSample of int * (string * Dba.LValue.t) list

type Ast.Instr.t += 
    | Sample of int * output list


include Cli.Make (struct
  let name = "XyntiaSampler"
  let shortname = "xyntiasampler"
end)

module Mode = Builder.Variant_choice_assoc (struct
  type t = out_type

  let name = "type"

  let doc =
    "Type of the samples output (xyntia or sygus; default: xyntia)"

  let default = Xyntia

  let assoc_map = [ ("xyntia", Xyntia); ("sygus", SyGus) ]
end)

module OutDir = Builder.String_option (struct
  let name = "out"
  let doc = "set output directory"
end)


module BVInterval = Interval.BitVec
type perm = { mutable readable: BVInterval.t ; mutable writable: BVInterval.t }

let pluginname = "termsampler"
     
let () = 
    (* Register the plugin with DBA instruction to set permissions *)
    Exec.register_plugin
    (module struct
      let name = "termsampler"

      let grammar_extension = [
          Dyp.Bind_to_cons 
            [
              ("out", "Obj");
              ("list_of_out", "Obj");
              ("res_out", "Obj");
            ];

          Dyp.Add_rules
            [
              (* Ask to sample some or all outputs *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "domain");
                    Dyp.Non_ter ("ident", No_priority);
                    Dyp.Regexp (RE_String "[");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String ",");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String "]");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _;
                      Libparser.Syntax.String var;
                      _;
                      Libparser.Syntax.Expr (Ast.Expr.Int (min, _), _); 
                      _;
                      Libparser.Syntax.Expr (Ast.Expr.Int (max, _), _); 
                      _;
                    ] -> (Libparser.Syntax.Decl (Domain (var, Z.to_int min, Z.to_int max)), [])
                  | _ -> assert false );
              (* Set the sampling domain for all the inputs *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "domain");
                    Dyp.Regexp (RE_String "all");
                    Dyp.Regexp (RE_String "[");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String ",");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String "]");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _; _;
                      Libparser.Syntax.Expr (Ast.Expr.Int (min, _), _); 
                      _;
                      Libparser.Syntax.Expr (Ast.Expr.Int (max, _), _); 
                      _;
                    ] -> (Libparser.Syntax.Decl (Domain ("all", Z.to_int min, Z.to_int max)), [])
                  | _ -> assert false );
              (* Set optimal sampling i.e., with fixed samples and 10% of full random (domains are kept for the remaining) *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "optimal");
                    Dyp.Regexp (RE_String "sampling");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _; ] -> (Libparser.Syntax.Decl OptimalSampling, [])
                  | _ -> assert false );
              (* Ask to prune outputs with no input i.e., which are constant *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "prune");
                    Dyp.Regexp (RE_String "constant");
                    Dyp.Regexp (RE_String "outputs");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _;] -> (Libparser.Syntax.Decl PruneCstOut, [])
                  | _ -> assert false );
              ( ( "fallthrough",
                  [
                    Dyp.Regexp (RE_String "sample");
                    Dyp.Non_ter ("const", No_priority);
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                  | [ _; Libparser.Syntax.Expr (Ast.Expr.Int (n, _), _); ] -> (Libparser.Syntax.Instr (Sample (Z.to_int n, [])), [])
                  | _ -> assert false );
              ( ( "fallthrough",
                  [
                    Dyp.Regexp (RE_String "sample");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Non_ter ("list_of_out", No_priority); (* list of outputs *)
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                  | [ 
                      _; 
                      Libparser.Syntax.Expr (Ast.Expr.Int (n, _), _);
                      Libparser.Syntax.Obj (OutList outs); 
                    ] -> (Libparser.Syntax.Instr (Sample (Z.to_int n, outs)), [])
                  | _ -> assert false );
              ( ( "out",
                  [ Dyp.Non_ter ("var", No_priority); ],
                  "default_priority",
                  [] ),
                fun _ -> function 
                    | [ Libparser.Syntax.Loc reg; ] -> (Libparser.Syntax.Obj (Out (Reg reg)), [])
                    | _ -> assert false);
              ( ( "list_of_out",
                  [
                    Dyp.Non_ter ("out", No_priority); 
                    Dyp.Regexp (RE_Char ',');
                    Dyp.Non_ter ("list_of_out", No_priority); 
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      Libparser.Syntax.Obj (Out out);
                      _;
                      Libparser.Syntax.Obj (OutList l);
                    ] -> (Libparser.Syntax.Obj (OutList (out::l)), [])
                  | _ -> assert false );
              ( ( "list_of_out",
                  [ Dyp.Non_ter ("out", No_priority); ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [ Libparser.Syntax.Obj (Out out); ] -> (Libparser.Syntax.Obj (OutList [out]), [])
                  | _ -> assert false );

              (* parse the out to put the samples *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "sample");
                    Dyp.Regexp (RE_String "output");
                    Dyp.Non_ter ("res_out", No_priority); (* directory or stdout *)
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                  | [
                      _; _; _; Libparser.Syntax.Obj (ResOut resout);
                    ] -> (Libparser.Syntax.Decl (SetOutput resout), [])
                  | _ -> assert false );
              ( ( "res_out",
                  [
                    Dyp.Regexp (RE_String "stdout"); (* standard output *)
                  ],
                  "default_priority",
                  [] ),
                fun _ _ -> (Libparser.Syntax.Obj (ResOut Stdout), []) );
              ( ( "res_out",
                  [
                      Dyp.Non_ter ("const", No_priority); 
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function 
                    | [ Libparser.Syntax.Expr (Ast.Expr.Bv outres, _); ] -> (* path are parsed as binary strings*)
                            (Libparser.Syntax.Obj (ResOut (Dir (Bitvector.to_asciistring outres))), [])
                    | _ -> assert false);
            ];
      ]

      let instruction_printer = None

      let declaration_printer = None

      let extension :
          type a b.
          (module EXPLORATION_STATISTICS) ->
          (module Path.S with type t = a) ->
          (module STATE with type t = b) ->
          (module Exec.EXTENSION with type path = a and type state = b) option =
       fun _stats path state ->
           let module MS = (val state) in 
           match is_enabled (), MS.Value.kind, MS.getter Senv.VisibleMemory, MS.getter Senv.VisibleSymbols with
           | true, Senv.Term, Some get_vmem, Some get_vsymb -> Some (module struct
                 module P = (val path)
                 module S = MS 
                 module Eval = Eval.Make (P) (S)

                 type path = P.t

                 and state = S.t

                 let resout = ref (match OutDir.get_opt ()  with 
                 | Some dir -> 
                        ignore (if Sys.file_exists dir then failwith "The output directory already exists"
                                else Sys.mkdir dir 0o777);
                        Dir dir  
                 | None -> Stdout)

                 let prune_cst_outputs = ref false
                 let already_sampled = ref false

                 type namemap = string Sexpr.BvTbl.t

                 let names : namemap = Sexpr.BvTbl.create 0
                 let nmem_var = ref 0
                 let nmem_out = ref 0
                 let optimal_sampling = ref false

                 type input_info = Sexpr.Expr.t * (Bitvector.t option)

                 (* id uniquely representing an output, name, expr, assigned value, inputs dependencies, size of the expression *)
                 type output_info = int * string * Sexpr.Expr.t * (Bitvector.t option) * input_info list * int option
                 
                 (* Hashmap from output id to list of output info *)
                 type samples = (int, output_info list) Hashtbl.t

                 let sampling : samples = Hashtbl.create 0

                 let domains : (string, int * int) Hashtbl.t = Hashtbl.create 0 
                 let domain_for_all = ref None

                 let initialization_callback = None

                 let instruction_callback =  Some (fun inst env ->
                    match inst with
                    | Sample (nsamples, outs) -> [ Ir.Builtin (BuiltinSample (nsamples, 
                      (List.map (function 
                      | Reg ((Var (name, _), _) as loc) -> name, Script.eval_loc loc env
                      | _ -> assert false) outs))) ]
                    | _ -> [])

                 let declaration_callback = Some (fun decl (_env : Script.env) _path state ->
                    match decl with
                    | SetOutput rout -> 
                            if Option.is_none (OutDir.get_opt ()) then (
                                resout := rout;
                                ignore (begin 
                                    match rout with 
                                    | Dir dir -> if Sys.file_exists dir then failwith "The output directory already exists"
                                                 else Sys.mkdir dir 0o777
                                    | _ -> () 
                                end));
                            Some state
                    | Domain ("all", min, max) -> 
                            domain_for_all := Some (min, max);
                            Some state

                    | Domain (varname, min, max) -> 
                            Hashtbl.add domains varname (min, max);
                            Some state
                    | OptimalSampling -> 
                            optimal_sampling := true;
                            Some state
                    | PruneCstOut ->
                            prune_cst_outputs := true;
                            Some state
                    | _ -> None)

                 let process_callback = None

                 let add_exprsize (id, name, expr, bv, inputs, esize) : output_info =
                    let _ = assert (Option.is_none esize) in
                    let rec aux e = match e with
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
                    | Sexpr.Expr.Cst _ -> 1
                    in
                    (id, name, expr, bv, inputs, Some (aux expr))

                 let add_inputs (id, name, expr, bv, inputs, esize) : output_info =
                    let _ =  assert (List.length inputs = 0) in
                    let rec aux e acc = match e with
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
                            let ({info; _ } : Dba.Var.t) = Dba.Var.create label ~bitsize:(Size.Bit.create size) ~tag:(Var.Tag.Empty) in
                            begin match info with
                            | Flag -> acc (* prune flags from the set of inputs *)
                            | _ ->
                                if not (Sexpr.BvTbl.mem names v) then
                                    Sexpr.BvTbl.add names v name;
                                if not (List.exists (fun (x, _) -> Sexpr.Expr.is_equal x v) acc) then
                                    (v, None)::acc
                                else
                                    acc
                            end
                    | Sexpr.Expr.Load _ as l -> 
                            if not (Sexpr.BvTbl.mem names l) then begin 
                                let mname = Printf.sprintf "mem_%d" !nmem_var in
                                nmem_var := !nmem_var + 1;
                                Sexpr.BvTbl.add names l mname; 
                            end;
                            if not (List.exists (fun (x, _) -> Sexpr.Expr.is_equal x l) acc) then
                                (l, None)::acc
                            else
                                acc
                    | Sexpr.Expr.Cst _ -> acc
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

                 let evaluate_input nsamples sampleid model (expr, _) : input_info =
                     let bv = Modeleval.eval ~symbols:(random_symb nsamples sampleid) ~memory:(random_mem nsamples sampleid) model expr in
                     (expr, Some bv)
                 
                 let evaluate_output nsamples sampleid model (id, name, expr, _, inputs, esize) : output_info = 
                     (* In theory by evaluating the output all the inputs are also set and so the model is already complete *)
                     let bv = Modeleval.eval ~symbols:(random_symb nsamples sampleid) ~memory:(random_mem nsamples sampleid) model expr in 
                     let inputs = List.fold_left (fun acc info -> (evaluate_input nsamples sampleid model info)::acc) [] inputs in 
                     (id, name, expr, Some bv, inputs, esize)


                 let get_regouts regmap acc = Dba_types.Var.Map.fold 
                                (fun key value acc -> ((List.length acc), key.name, value, None, [], None)::acc)
                                regmap acc
                     

                 let get_memouts lmap acc = 
                     let rec aux (lmap : Sexpr.Memory.t) acc = match lmap with
                     | Layer {over=next;addr=base;store; _} as layer -> 
                             let acc = Sexpr.Store.fold_term 
                                    (fun offset expr cumul -> 
                                        let endianess = Kernel_options.Machine.endianness () in
                                        let sizeofaddr = Kernel_options.Machine.word_size () in
                                        let loadaddr = Sexpr.Expr.add base (Sexpr.Expr.constant (Bitvector.create offset sizeofaddr)) in
                                        let bytesize = (Sexpr.Expr.sizeof expr) / 8 in
                                        let loadexpr = Sexpr.Expr.load bytesize endianess loadaddr layer in
                                        assert (not (Sexpr.BvTbl.mem names loadexpr));
                                        let mname = Printf.sprintf "outmem_%d" !nmem_out in
                                        nmem_out := !nmem_out + 1;
                                        Sexpr.BvTbl.add names loadexpr mname; 
                                        ((List.length cumul), mname, expr, None, [], None)::cumul
                                    ) acc store in
                             aux next acc
                     | Symbol _ | Root -> acc
                     in
                     aux lmap acc

                 let tojson (outsinfos : output_info list) = 
                     let samp = List.fold_left (fun acc info -> 
                                let _, oname, _, obv, inputs, _ = info in
                                let obv = Option.get obv in
                                let ohexsize = Printf.sprintf "0x%x" ((Bitvector.size_of obv)) in
                                let toadd = (Int.to_string (List.length acc), 
                                                `Assoc [ 
                                                    ("inputs", `Assoc (List.fold_left (fun acc2 input -> 
                                                        let iexpr, ibv = input in
                                                        let iloc = Sexpr.BvTbl.find names iexpr in
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
                            ) [] outsinfos in
                     let _, _, _, _, _, exprsize = List.hd outsinfos in 
                     `Assoc [ 
                         ("initial", snd (List.hd samp)); 
                         ("sampling", `Assoc samp) ; 
                         ("info"), `Assoc [ "exprsize", `Int (Option.get exprsize) ]
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
                              let iloc = Sexpr.BvTbl.find names iexpr in
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
                                let iloc = Sexpr.BvTbl.find names iexpr in
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

                 let sample nsamples (outs : (string * Dba.LValue.t) list) _ _path _ state : (S.t, status) Result.t =
                         if !already_sampled then failwith "[Error] multiple branch";
                         already_sampled := true;

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
                             | _ -> true) (get_vsymb state) 
                         in

                         (* Get all outputs *)
                         let outsinfo = get_regouts symbols [] in
                         let outsinfo = get_memouts (get_vmem state) outsinfo in
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
                         let nsuccess_sampling = ref 0 in
                         for sampleid=0 to ntests-1 do
                            if !nsuccess_sampling < nsamples then (
                                (* Evaluate inputs *)
                                let model = Sexpr.Model.empty (Kernel_options.Machine.word_size ()) in
                                let evaluated_outsinfo = begin
                                    try 
                                        Some (List.fold_left (fun acc info -> (evaluate_output nsamples sampleid model info)::acc) [] outsinfo)
                                    with Division_by_zero -> None
                                    
                                end in

                                begin match evaluated_outsinfo with 
                                | Some e -> 
                                    nsuccess_sampling := !nsuccess_sampling + 1;
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

                         ignore (match !resout with
                         | Dir dir -> 
                                  (* Get all read expression with their name *)
                                  let map_names = Sexpr.BvTbl.fold (fun expr name acc -> 
                                    begin match expr with 
                                    | Load _ -> Basic_types.String.Map.add name expr acc
                                    | _ -> acc end) names Basic_types.String.Map.empty in

                                let map_names = List.fold_left (fun acc (_, name, e, _, _, _) -> 
                                  Basic_types.String.Map.add name e acc
                                ) map_names outsinfo in
                                
                                let slice = Basic_types.String.Map.fold (fun name e acc -> (e, name)::acc) map_names [] in
                                (* dump sliced formula to file *)
                                let ochan = open_out (String.concat "/" [ dir; "formula" ]) in
                                let fmt = (Format.formatter_of_out_channel ochan) in
                                S.pp_smt (Some slice) fmt state; 
                                Format.pp_print_flush fmt ();
                                close_out ochan
                         | _ -> ());
                         Ok state

                 let builtin_callback = Some (function
                     | BuiltinSample (nsamples, outs) -> Some (sample nsamples outs)
                     | _ -> None)

                 let builtin_printer = None

                 let at_exit_callback = Some (fun () -> 
                    match Mode.get () with
                    | Xyntia ->
                      Hashtbl.iter (fun oid outsinfos -> 
                                  let json = tojson outsinfos in         
                                  match !resout with
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
                        | Stdout -> Printf.printf "%s\n" constrs
                        | Dir dir -> 
                                let id, name, _, _, _, _ = List.hd outsinfos in
                                assert (oid = id);
                                let fname = Printf.sprintf "%s/%s.sl" dir name in 
                                let file = open_out fname in
                                Printf.fprintf file "%s\n" constrs;
                                flush file;
                                close_out file
                              ) sampling)
               end)
            | _ -> None
    end : Exec.PLUGIN)
