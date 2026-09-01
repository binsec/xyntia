open Xyntia
open Xyntia_utils

let () = 
    (* Initialize the seed (required as Xyntia performs stochastic search) *)
    Random.self_init ();

    (* Module to easily define expressions *)
    let module TS = Treeshortcut.UnitTreeShortCut in
    
    (* Create input and output variables *)
    let x = Tree.var_of "x" 32 in
    let y = Tree.var_of "y" 32 in
    let res = Tree.var_of "res" 32 in

    (* Create the input vectors on which we will run our expression *)
    let samples = auto_sample [| x; y |] in

    (* Create the expression to synthesize *)
    let xt : unit Tree.t = x |> Tree.mk_var () in
    let yt : unit Tree.t = y |> Tree.mk_var () in
    let c : unit Tree.t = Tree.mk_const_of_bv () (Bitvector.of_int ~size:32 1000) in
    let newtree = TS.(xt + yt + c) in
    
    (* Create the oracle i.e., run the expression over the provided inputs *)
    let module O = (val (Oracle.of_expr newtree samples res [||])) in

    Printf.printf "%s" (Oracle.print_samples (O.get_samples ()));

    (* Create the set of operators that will be used during synthesis *)
    let (module MU : Mutations.MUTATIONS) = Mutations.mutations_of_str (module O: Oracle.ORACLE) "mba" in

    (* Define the arguments for Xyntia *)
    let synth_args : synthesis_args = { 
            default_args with infrules = all_inference_rules
    } in

    (* Run Xyntia *)
    let smry = run (module O) (module MU) synth_args in
    Printf.printf "%s\n" (Tree.string_of smry.synthesis.simplified);

