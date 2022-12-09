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

(** Results summary i.e. what is returned by S-metaheuristics **)
type summary = {
    success : bool ;
    expression : string ;
    simplified : string ;
    smtlib : string ;
    time_synthesis : float ;
    time_simplify : float;
  }

module type Dist = sig
  val dist : Bitvector.t -> Bitvector.t -> float
  val is_zero : float -> bool
end

module type S = sig
  val search : int -> summary
end

type solution = { tree : Tree.t ; cost : float }

type sum_array = { mutable sum : float }

exception Halt

let no_time_message = "Not enough time to start synthesis"

(** Computes (dist ar.(0) ar'.(0)) + ... + (dist ar.(n) ar'.(n))**)
let sum_dists dist ar ar' =
    let acc = { sum = 0. } in
    Array.iter2 (fun x y -> acc.sum <- acc.sum +. dist x y) ar ar';
    acc.sum

(** Module for the Iterated Local Search S-metaheuristic **)
module Mk_iterated_local_search (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S = struct
    module M = Tree.Mk_Mutator (O) (MU)

    let is_stuck n = n > 100 (** number of mutations where no improvement is observed before doing a perturbation **)

    (** Compute the cost of an AST i.e. 
      * the distance between its outputs 
      * and the observed ones **)
    let cost tree = 
        let actual_outs = M.eval tree in
        let exp_outs = O.out_values () in
        sum_dists D.dist actual_outs exp_outs

    let gen_sol tree = { tree ; cost = cost tree } (** a solution is a tree (i.e. an AST) with its cost **)

    (** Implementation of the Iterated Local Search algorithm **)
    let search maxdepth =
        let time_base = Sys.time() in (* Initialize time_base to enforce timeout *)
        try

            (* Perturbation step *)
            let perturbate sol =
                let num_cuts = 2 in (* Number of time to perturbate the AST *)
                let rec perturbate_loop n t =
                    if n = num_cuts then t
                    else perturbate_loop (n + 1) (M.cut t)
                in
                perturbate_loop 0 sol.tree
            in

            (* Mutate the AST iteratively
             * sol: the AST
             * n : number of non progressing iterations
             * depth : states how many perturbations have been done (use to reset the possible mutation choices) *)
            let rec loop sol n depth =
                (* Iterative mutations of the AST *)
                if is_stuck n || D.is_zero sol.cost then sol
                else
                    let mutated = M.mutate maxdepth sol.tree (Some depth) in
                    let sol' = gen_sol mutated in
                    if sol'.cost < sol.cost then loop sol' 0 depth
                    else loop sol (n + 1) depth 
            in

            Random.self_init ();
            let best_sol = ref (gen_sol (M.singleton ())) in (* Initial state: an AST of size 1 i.e. a constant or a variable *)
            let depth = ref 0 in (* keep track on the number of perturbation *)
            try
                while not (D.is_zero !best_sol.cost) do
                    (* Iterate until the distance equals 0 or timeout *)
                    let perturbated_sol = gen_sol (perturbate !best_sol) in
                    let sol' = loop perturbated_sol 0 !depth in
                    depth := !depth + 1;
                    best_sol := if sol'.cost < !best_sol.cost then sol' else !best_sol
                done;

                let time_synth = Sys.time() in  
                let simpl = M.simplify !best_sol.tree in (* Postprocess to clean the synthesized expression *)
                let time_simpl = Sys.time() in  
                let simpl_sol = gen_sol simpl in

                Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> ()));

                (* Check that the postprocess did not changed the observable behaviors *)
                if (D.is_zero !best_sol.cost) && not (D.is_zero simpl_sol.cost) then 
                    (* Postprocess changed the observable behaviors: there is a bug in the postprocess *)
                    failwith (Printf.sprintf "%s != %s\n" (M.to_string !best_sol.tree) (M.to_string simpl_sol.tree));

                { 
                    (* Return the solution *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree; 
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = time_synth -. time_base; 
                    time_simplify = time_simpl -. time_synth;
                }
            with Halt ->
                let time_synth = Sys.time() in  
                let simpl = (M.simplify !best_sol.tree) in
                let time_simpl = Sys.time() in  
                { 
                    (* Return Timeout *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = time_synth -. time_base; 
                    time_simplify = time_simpl -. time_synth;
                }

        with Halt -> failwith no_time_message
end

(** Module for the Random Walk S-metaheuristic **)
module Mk_random_walk (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S = struct
    module M = Tree.Mk_Mutator (O) (MU)

    (** Compute the cost of an AST i.e. 
      * the distance between its outputs 
      * and the observed ones **)
    let cost tree =
        let actual_outs = M.eval tree in
        let exp_outs = O.out_values () in
        sum_dists D.dist actual_outs exp_outs

    let gen_sol tree = { tree ; cost = cost tree }

    (** Implementation of the Random Walk algorithm **)
    let search maxdepth =
        try
            Random.self_init ();

            let depth = ref 0 in

            let best_sol = ref (gen_sol (M.singleton ())) in (* Initial state: an AST of size 1 i.e. a constant or a variable *)

            let rec loop sol =
                if D.is_zero !best_sol.cost then ()
                else
                    let sol' = gen_sol (M.mutate maxdepth sol.tree None) in
                    best_sol := if sol'.cost < !best_sol.cost then sol' else !best_sol;
                    depth := !depth +1;
                    loop sol'
            in
            try
                loop !best_sol;
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return the solution *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }
            with Halt ->
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return Timeout *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }

        with Halt -> failwith no_time_message
end

(** Module for the Hill Climbing S-metaheuristics **)
module Mk_hill_climbing (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S = struct
    module M = Tree.Mk_Mutator (O) (MU)

    let is_stuck n = n > 100

    (** Compute the cost of an AST i.e. 
      * the distance between its outputs 
      * and the observed ones **)
    let cost tree =
        let actual_outs = M.eval tree in
        let exp_outs = O.out_values () in
        sum_dists D.dist actual_outs exp_outs

    let gen_sol tree = { tree ; cost = cost tree }

    (** Implementation of the Hill Climbing algorithm **)
    let search maxdepth =
        try
            Random.self_init ();
            let best_sol =
                ref (gen_sol (M.singleton ())) (* Initial state: an AST of size 1 i.e. a constant or a variable *)
            in

            let rec loop sol n =
                if D.is_zero !best_sol.cost then ()
                else if is_stuck n then loop (gen_sol (M.singleton ())) 0
                else
                    let sol' = gen_sol (M.mutate maxdepth !best_sol.tree None) in
                    if sol'.cost < !best_sol.cost then 
                        best_sol := sol';
                    if sol'.cost < sol.cost then loop sol' 0
                    else loop sol (n + 1)
            in
            try 
                loop !best_sol 0;
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return the solution *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }
            with Halt ->
                let simpl = M.simplify !best_sol.tree in
                {
                    (* Return Timeout *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }

        with Halt -> failwith no_time_message
end

(** Module for the Simulated Annealing S-metaheuristic **)
module Mk_simulated_annealing (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S = struct
    module M = Tree.Mk_Mutator (O) (MU)

    (** Compute the cost of an AST i.e. 
      * the distance between its outputs 
      * and the observed ones **)
    let cost tree =
        let actual_outs = M.eval tree in
        let exp_outs = O.out_values () in
        sum_dists D.dist actual_outs exp_outs

    let gen_sol tree = { tree ; cost = cost tree }

    (* rate of the geometric rate cooling *)
    let alpha = 0.8

    (* factor for determining batch size *)
    let factor = 100

    (* we stop if we don't improve the best solution
         after this number of batches *)
    let is_stuck n = n > 2

    let initial_temp maxdepth () =
        let acceptance = 0.90 in
        let lim = 100 in
        let rec loop acc count sol =
            if count = lim then acc /. float count
            else
                let sol' = gen_sol (M.mutate maxdepth sol.tree None) in
                let delta = sol'.cost -. sol.cost in
                if delta > 0. then loop (acc +. delta) (count + 1) sol'
                else loop acc count sol'
        in

        let sol = gen_sol (M.singleton ()) in
        -.(loop 0. 0 sol) /. log acceptance

    let replace temp sol sol' =
        Random.float 1. < exp((sol.cost -. sol'.cost) /. temp)

    (** Implementation of the Simulated annealing algorithm **)
    let search maxdepth =
        try
            Random.self_init ();
            let t0 = initial_temp maxdepth () in
            let t_lim = t0 /. 1000. in

            (* batch size should be proportional to the neighborhood size,
                which in our case is proportional to the number of nodes in
                the tree *)
            let batch_size = factor * (1 lsl maxdepth) in

            let best_sol = ref (gen_sol (M.singleton ())) in (* Initial state: an AST of size 1 i.e. a constant or a variable *)

            let rec loop temp sol count =
                if D.is_zero !best_sol.cost then
                    ()
                else if temp < t_lim || is_stuck count then
                    batch_loop t0 (gen_sol (M.singleton ())) 1 0
                else
                    batch_loop temp sol count 0

            and batch_loop temp sol count n =
                if n = batch_size then
                    loop (temp *. alpha) sol (count + 1)
                else
                    let sol' = gen_sol (M.mutate maxdepth sol.tree None) in
                    let (best_sol', count) =
                        if sol'.cost < !best_sol.cost then (sol', 0)
                        else (!best_sol, count)
                    in
                    best_sol := best_sol';
                    if replace temp sol sol' then
                        batch_loop temp sol' count (n + 1)
                    else
                        batch_loop temp sol count (n + 1)
            in

            try
                loop t0 !best_sol 1;
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return the solution *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }
            with Halt ->
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return Timeout *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl;
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }

        with Halt -> failwith no_time_message
end


(** Module for the Metropolis Hasting S-metaheuristic **)
module Mk_metropolis_hastings (D : Dist) (O : Oracle.ORACLE) (MU : Tree.MUTATIONS) : S = struct
    module M = Tree.Mk_Mutator (O) (MU)

    (** Compute the cost of an AST i.e. 
      * the distance between its outputs 
      * and the observed ones **)
    let cost tree =
        let actual_outs = M.eval tree in
        let exp_outs = O.out_values () in
        sum_dists D.dist actual_outs exp_outs

    let gen_sol tree = { tree ; cost = cost tree }

    (* rate of the geometric rate cooling *)
    let alpha = 0.8

    (* factor for determining batch size *)
    let factor = 100

    (* we stop if we don't improve the best solution
         after this number of batches *)
    let is_stuck n = n > 2

    let initial_temp maxdepth () =
        let acceptance = 0.90 in
        let lim = 100 in
        let rec loop acc count sol =
            if count = lim then acc /. float count
            else
                let sol' = gen_sol (M.mutate maxdepth sol.tree None) in
                let delta = sol'.cost -. sol.cost in
                if delta > 0. then loop (acc +. delta) (count + 1) sol'
                else loop acc count sol'
        in
        let sol = gen_sol (M.singleton ()) in
        -.(loop 0. 0 sol) /. log acceptance

    let replace temp sol sol' p =
        Random.float 1. < p *. exp((sol.cost -. sol'.cost) /. temp)

    (** Implementation of the Metropolis Hasting algorithm **)
    let search maxdepth =
        try
            Random.self_init ();
            let t0 = initial_temp maxdepth () in
            let t_lim = t0 /. 1000. in
            (* batch size should be proportional to the neighborhood size,
                which in our case is proportional to the number of nodes in
                the tree *)
            let batch_size = factor * (1 lsl maxdepth) in
            let best_sol = ref (gen_sol (M.singleton ())) in (* Initial state: an AST of size 1 i.e. a constant or a variable *)
            let rec loop temp sol count =
                if D.is_zero !best_sol.cost then
                    ()
                else if temp < t_lim || is_stuck count then
                    batch_loop t0 (gen_sol (M.singleton ())) 1 0
                else
                    batch_loop temp sol count 0
            and batch_loop temp sol count n =
                if n = batch_size then
                    loop (temp *. alpha) sol (count + 1)
                else
                    let (t', p) = M.mutate_mh maxdepth sol.tree None in
                    let sol' = gen_sol t' in
                    let (best_sol', count) =
                        if sol'.cost < !best_sol.cost then (sol', 0)
                        else (!best_sol, count)
                    in
                    best_sol := best_sol';
                    if replace temp sol sol' p then
                        batch_loop temp sol' count (n + 1)
                    else
                        batch_loop temp sol count (n + 1)
            in

            try
                loop t0 !best_sol 1;
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return the solution *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl; 
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }
            with Halt ->
                let simpl = M.simplify !best_sol.tree in
                { 
                    (* Return Timeout *)
                    success = D.is_zero !best_sol.cost ;
                    expression = M.to_string !best_sol.tree;
                    simplified = M.to_string simpl;
                    smtlib = M.to_smtlib simpl; 
                    time_synthesis = 0.0;
                    time_simplify = 0.0;
                }

        with Halt -> failwith no_time_message
end

(** Arithmetic distance **)
let mk_arith () =
    (module struct
        let dist x y = Float.abs (Float.of_int ((Bitvector.to_int x) - (Bitvector.to_int y)))

        let is_zero x = abs_float x < 0.5
    end : Dist)

(** Hamming distance **)
let mk_hamming () =
    (module struct
        let dist x y =
            let rec nb_ones x i = 
                let bit = 
                    if Bitvector.get_bit x i then
                        1
                    else 
                        0
                in
                if i <> 0 then
                    bit + (nb_ones x (i-1))
                else
                    bit
            in
            let xored = Bitvector.logand (Bitvector.logor x y) (Bitvector.lognot (Bitvector.logand x y)) in
            let hamming = nb_ones xored ((Bitvector.size_of x)-1) in
            Float.of_int hamming
        (*
        let dist x y =
            let rec aux acc v =
                if Bitvector.is_zero v then acc
                else
                    aux (acc + 1) (Bitvector.logand v (Bitvector.sub v Bitvector.one))
            in
            float (aux 0 (Bitvector.sub x y))
        *)

        let is_zero x = abs_float x < 0.5 
    end : Dist)

(** Xor distance **)
let mk_xor () =
    (module struct
        let dist x y =
            let xored = Bitvector.logand (Bitvector.logor x y) (Bitvector.lognot (Bitvector.logand x y)) in
            Bitvector.to_float xored

        let is_zero x = abs_float x < 0.5
    end : Dist)

(** Logarithmetic distance **)
let mk_logarith () =
    (module struct
        let dist x y = 
            let arithme = Float.abs (Float.of_int ((Bitvector.to_int x) - (Bitvector.to_int y))) in
            (Float.log (arithme +. 1.)) /. (Float.log 2.) 

        let is_zero x = abs_float x < 0.5
    end : Dist)

(** Distance used by Syntia in the paper "Syntia: Synthesizing the Semantics of Obfuscated Code" by Blazytko et al. **)
let mk_syntia () =
    (module struct
        let dist_arith x y = 
            let n = Float.abs (Float.of_int ((Bitvector.to_int x) - (Bitvector.to_int y))) in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            let divisor = (2. ** fl_sz) -. 1. in
            n /. divisor 

        let dist_hamm x y =
            let rec nb_ones x i = 
                let bit = 
                    if Bitvector.get_bit x i then
                        1
                    else 
                        0
                in
                if i <> 0 then
                    bit + (nb_ones x (i-1))
                else
                    bit
            in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            let xored = Bitvector.logand (Bitvector.logor x y) (Bitvector.lognot (Bitvector.logand x y)) in
            let hamming = nb_ones xored ((Bitvector.size_of x)-1) in
            (Float.of_int hamming) /. fl_sz

        let trailing_ones x y =
            let rec last_ones x i =
                let bit = 
                    if Bitvector.get_bit x i then
                        1 
                    else
                        0
                in
                match bit, i with
                | _, v when v = (Bitvector.size_of x) -1 -> bit
                | 0, _ -> 0
                | 1, _ -> 1 + last_ones x (i+1)
                | _, _ -> assert false
            in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            (Float.abs ((Float.of_int (last_ones x 0)) -. (Float.of_int (last_ones y 0)))) /. fl_sz

        let trailing_zeros x y =
            let rec last_zeros x i =
                let bit = 
                    if Bitvector.get_bit x i then
                        1
                    else
                        0 
                in
                match bit, i with
                | _, v when v = (Bitvector.size_of x) -1 -> bit
                | 1, _ -> 0
                | 0, _ -> 1 + last_zeros x (i+1)
                | _, _ -> assert false
            in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            (Float.abs ((Float.of_int (last_zeros x 0)) -. (Float.of_int (last_zeros y 0)))) /. fl_sz

        let leading_ones x y =
            let rec first_ones x i =
                let bit = 
                    if Bitvector.get_bit x i then
                        1 
                    else
                        0
                in
                match bit, i with
                | _, 0 -> bit
                | 0, _ -> 0
                | 1, _ -> 1 + first_ones x (i-1)
                | _, _ -> assert false
            in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            (Float.abs ((Float.of_int (first_ones x ((Bitvector.size_of x)-1))) -. (Float.of_int (first_ones y ((Bitvector.size_of y)-1))))) /. fl_sz

        let leading_zeros x y =
            let rec first_zeros x i =
                let bit = 
                    if Bitvector.get_bit x i then
                        1 
                    else
                        0
                in
                match bit, i with
                | _, 0 -> bit
                | 1, _ -> 0
                | 0, _ -> 1 + first_zeros x (i-1)
                | _, _ -> assert false
            in
            let fl_sz = Float.of_int (Bitvector.size_of x) in
            (Float.abs ((Float.of_int (first_zeros x ((Bitvector.size_of x)-1))) -. (Float.of_int (first_zeros y ((Bitvector.size_of y)-1))))) /. fl_sz

        let dist x y = ((dist_arith x y) +. (dist_hamm x y) +. (trailing_zeros x y) +. (trailing_ones x y) +. (leading_zeros x y) +. (leading_ones x y)) /. 6.


        let is_zero x = abs_float x = 0.0
    end : Dist)

(** Select from a string which distance to use **)
let of_string (module D : Dist) (module O : Oracle.ORACLE) (module MU : Tree.MUTATIONS) = function
| "ils" -> (module Mk_iterated_local_search (D) (O) (MU) : S)
| "hc" -> (module Mk_hill_climbing (D) (O) (MU) : S)
| "rw" -> (module Mk_random_walk (D) (O) (MU) : S)
| "sa" -> (module Mk_simulated_annealing (D) (O) (MU) : S)
| "mh" -> (module Mk_metropolis_hastings (D) (O) (MU) : S)
| _ -> invalid_arg "Undefined heuristic option"
