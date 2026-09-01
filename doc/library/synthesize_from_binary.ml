open Xyntia
open Xyntia_utils

let () = 
    (* Initialize the seed (required as Xyntia performs stochastic search) *)
    Random.self_init ();

    let binary = "binary" in
    let config = "config.ini" in
    
    (* Use Binsec to sample and extract the formula of the binary *)
    Sampler.sample binary config None None;
    let samples = Sampler.get_samples () in
    let formula = Sampler.get_formula () in
    
    (* Define the arguments for Xyntia *)
    let synth_args : synthesis_args = { default_args with
        formula = formula; (* Set to the formula extracted from Binsec *)
        check = true; (* Set to true to check the equivalence with the groundtruth *)
    } in

    (* Run synthesis for each output detected by Binsec *)
    List.iter (fun sample -> 
        (* Create the oracle with the provided input-output examples *)
        let (module O : Oracle.ORACLE) = match (sample : Sampler.samplekind) with 
        | File f -> Oracle.of_json_file ~filename:f [||]
        | Json j -> Oracle.of_json j [||]
        in
         (* Create the set of operators that will be used during synthesis *)
        let (module MU : Mutations.MUTATIONS) = Mutations.mutations_of_str (module O: Oracle.ORACLE) "mba" in
        (* Run Xyntia *)
        let smry = run (module O) (module MU) synth_args in
        Printf.printf "%s\n" (Tree.string_of smry.synthesis.simplified);
        Printf.printf "equiv: %s\n" (Checker.answer_to_string smry.equiv);
    ) samples;
