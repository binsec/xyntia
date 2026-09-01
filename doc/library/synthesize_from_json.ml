
open Xyntia
open Xyntia_utils

let () = 
    (* Initialize the seed (required as Xyntia performs stochastic search) *)
    Random.self_init ();

    let jsonfile = "yourfile.json" in
    
    (* Create the oracle with the provided input-output examples *)
    let module O = (val (Oracle.of_json_file ~filename:jsonfile [||])) in

    (* Create the set of operators that will be used during synthesis *)
    let (module MU : Mutations.MUTATIONS) = Mutations.mutations_of_str (module O: Oracle.ORACLE) "mba" in

    (* Define the arguments for Xyntia *)
    let synth_args : synthesis_args = default_args in

    (* Run Xyntia *)
    let smry = run (module O) (module MU) synth_args in
    Printf.printf "%s\n" (Tree.string_of smry.synthesis.simplified);
