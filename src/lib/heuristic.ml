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

type kind = ILS | RW | HC | MH | SA 

(** Results summary i.e. what is returned by S-metaheuristics **)
type summary = {
    success : bool ;
    expression : unit Tree.t;
    simplified : unit Tree.t;
    smtlib : string ;
    size : int ;
    time_synthesis : float ;
    time_simplify : float ;
    nb_mutations : int ;
}

module type DIST = Distance.DIST
module type VECDIST = Distance.VECDIST

module type S = sig
  val search : int -> summary
end


exception Halt = Exceptions.Halt
exception CEGISHalt = Exceptions.CEGISHalt
exception SynthesisNotStarted = Exceptions.SynthesisNotStarted

type 'a solution = { tree : 'a Tree.t ; cost : float ; final_cost : float }

module type BaseSearch = sig
    val search : terminated: (Mutations.mutd solution -> bool) -> 
                 gen_sol: (Mutations.mutd Tree.t -> Mutations.mutd solution) ->  
                 best_sol: Mutations.mutd solution ref -> 
                 maxdepth: int -> 
                 bool
end

module MkSearch (BSF: functor (M : Mutations.MUTATOR) -> BaseSearch) (M : Mutations.MUTATOR) 
    (GD : VECDIST with type t = M.data) (FD : VECDIST with type t = M.data)
    (O : Oracle.ORACLE) : S = struct
    module Simp = Simplifier.TreeSimplifier (M) (O)
    module BS = BSF (M)

    let terminated sol = 
        FD.is_zero sol.final_cost

    (* Compute the cost of an AST i.e. 
     * the distance between its outputs 
     * and the observed ones *)
    let cost tree = 
        let actual = M.eval tree in
        let expected = O.out_values () in
        GD.vecdist tree actual expected
    
    let final_cost tree = 
        let actual = M.eval tree in
        let expected = O.out_values () in
        FD.vecdist tree actual expected

    let mutnb = ref 0

    let gen_sol tree = 
        incr mutnb;
        { tree ; cost = cost tree ; final_cost = final_cost tree }

    let search maxdepth =
        mutnb := 0;
        let time_base = Sys.time() in

        try
            let best_sol = ref (gen_sol (M.singleton ())) in (* Initial state: an AST of size 1 i.e. a constant or a variable *)

            let finished = 
                try
                    BS.search ~terminated ~gen_sol ~best_sol ~maxdepth
                with Halt | CEGISHalt -> false
            in
                
            let actual = M.eval !best_sol.tree in
            let expected = O.out_values () in

            if finished then (
                (* Convert the resulting solution if we activated top-level constant analysis *)
                best_sol := gen_sol (FD.extract !best_sol.tree actual expected);

                (* Checks that the extraction did not broke the solution *)
                if (not (terminated !best_sol)) then
                    failwith (Printf.sprintf "pattern does not zeroify %s\n" (M.to_string !best_sol.tree))
            );

            let time_synth = Sys.time() in
            let simpl = Simp.simplify !best_sol.tree in (* Postprocess to clean the synthesized expression *)
            let time_simpl = Sys.time() in  
            let simpl_sol = gen_sol simpl in

            (* Disable the timeout *)
            Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> ()));

            (* Check that the postprocess did not changed the observable behaviors *)
            if finished && (terminated !best_sol) && not (terminated simpl_sol) then 
                (* Postprocess changed the observable behaviors: there is a bug in the postprocess *)
                failwith (Printf.sprintf "%s != %s\n" (M.to_string !best_sol.tree) (M.to_string simpl_sol.tree));

            { 
                (* Return the solution *)
                success = terminated !best_sol ;
                expression = Tree.UnitTreeConstructor.transfer !best_sol.tree; 
                simplified = Tree.UnitTreeConstructor.transfer simpl;
                smtlib = M.to_smtlib simpl;
                size = M.get_expr_size simpl;
                time_synthesis = time_synth -. time_base; 
                time_simplify = time_simpl -. time_synth;
                nb_mutations = !mutnb;
            }
        with Halt | CEGISHalt -> raise SynthesisNotStarted
end        

(** Module for the Iterated Local Search S-metaheuristic **)
module Iterated_local_search (M : Mutations.MUTATOR) : BaseSearch = struct
    let is_stuck n = n > 100 (** number of mutations where no improvement is observed before doing a perturbation **)

    (* Perturbation step *)
    let perturbate sol =
        let num_cuts = 2 in (* Number of time to perturbate the AST *)
        let rec perturbate_loop n t =
            if n = num_cuts then t
            else perturbate_loop (n + 1) (M.cut t)
        in
        perturbate_loop 0 sol.tree

    let search ~terminated ~gen_sol ~best_sol ~maxdepth =
        (* Mutate the AST iteratively
        * sol: the AST
        * n : number of non progressing iterations
        * depth : states how many perturbations have been done (use to reset the possible mutation choices) *)
        let rec loop sol n depth =
            (* Iterative mutations of the AST *)
            if is_stuck n || terminated sol then sol
            else
                let mutated = M.mutate maxdepth sol.tree None in
                let sol' = gen_sol mutated in
                if terminated sol' then sol'
                else if sol'.cost < sol.cost then loop sol' 0 depth
                else loop sol (n + 1) depth 
        in
        let rec iteration best_sol depth =
            (* Iterate until the distance equals 0 or timeout *)
            if terminated !best_sol then ()
            else
                let perturbated_sol = gen_sol (perturbate !best_sol) in
                let sol' = loop perturbated_sol 0 depth in
                best_sol := if (terminated sol') || (sol'.cost < !best_sol.cost) then sol' else !best_sol;
                iteration best_sol (depth + 1)
        in
        iteration best_sol 0;
        true
end

(** Module for the Random Walk S-metaheuristic **)
module Random_walk (M : Mutations.MUTATOR) : BaseSearch = struct
    let search ~terminated ~gen_sol ~best_sol ~maxdepth =
        let rec loop sol =
            if terminated !best_sol then ()
            else
                let sol' = gen_sol (M.mutate maxdepth sol.tree None) in
                best_sol := if (terminated sol') || (sol'.cost < !best_sol.cost) then sol' else !best_sol;
                loop sol'
        in
        loop !best_sol;
        true
end

(** Module for the Hill Climbing S-metaheuristics **)
module Hill_climbing (M : Mutations.MUTATOR) : BaseSearch = struct
    let is_stuck n = n > 100

    let search ~terminated ~gen_sol ~best_sol ~maxdepth =
        let rec loop sol n =
            if terminated !best_sol then ()
            else if is_stuck n then loop (gen_sol (M.singleton ())) 0
            else
                let sol' = gen_sol (M.mutate maxdepth !best_sol.tree None) in
                if terminated sol' || (sol'.cost < !best_sol.cost) then 
                    best_sol := sol';
                if sol'.cost < sol.cost then loop sol' 0
                else loop sol (n + 1)
        in
        loop !best_sol 0;
        true
end

(** Module for the Simulated Annealing S-metaheuristic **)
module Simulated_annealing (M : Mutations.MUTATOR) : BaseSearch = struct
    (* rate of the geometric rate cooling *)
    let alpha = 0.8

    (* factor for determining batch size *)
    let factor = 100

    (* we stop if we don't improve the best solution
         after this number of batches *)
    let is_stuck n = n > 2

    let replace temp sol sol' =
        Random.float 1. < exp((sol.cost -. sol'.cost) /. temp)

    let search ~terminated ~gen_sol ~best_sol ~maxdepth =
        let t0 =
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
        in
        let t_lim = t0 /. 1000. in

        (* batch size should be proportional to the neighborhood size,
            which in our case is proportional to the number of nodes in
            the tree *)
        let batch_size = factor * (1 lsl maxdepth) in
        
        let rec loop temp sol count =
            if terminated !best_sol then ()
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
                    if terminated sol' || (sol'.cost < !best_sol.cost) then (sol', 0)
                    else (!best_sol, count)
                in
                best_sol := best_sol';
                if replace temp sol sol' then
                    batch_loop temp sol' count (n + 1)
                else
                    batch_loop temp sol count (n + 1)
        in
        loop t0 !best_sol 1;
        true
end


(** Module for the Metropolis Hasting S-metaheuristic **)
module Metropolis_hastings (M : Mutations.MUTATOR) : BaseSearch = struct    
    (* rate of the geometric rate cooling *)
    let alpha = 0.8

    (* factor for determining batch size *)
    let factor = 100

    (* we stop if we don't improve the best solution
         after this number of batches *)
    let is_stuck n = n > 2

    let replace temp sol sol' p =
        Random.float 1. < p *. exp((sol.cost -. sol'.cost) /. temp)

    let search ~terminated ~gen_sol ~best_sol ~maxdepth =
        let t0 =
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
        in
        let t_lim = t0 /. 1000. in
        (* batch size should be proportional to the neighborhood size,
            which in our case is proportional to the number of nodes in
            the tree *)
        let batch_size = factor * (1 lsl maxdepth) in
        (* @TODO (NB): Could be transformed into immutable solutions (I think) *)
        let rec loop temp sol count =
            if terminated !best_sol then ()
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
                    if terminated sol' || (sol'.cost < !best_sol.cost) then (sol', 0)
                    else (!best_sol, count)
                in
                best_sol := best_sol';
                if replace temp sol sol' p then
                    batch_loop temp sol' count (n + 1)
                else
                    batch_loop temp sol count (n + 1)
        in
        loop t0 !best_sol 1;
        true
end

let kind_of_string = function 
| "ils" -> ILS
| "hc" -> HC
| "rw" -> RW
| "sa" -> SA
| "mh" -> MH
| _ -> invalid_arg "Undefined heuristic option" 

(** Select from a {!kind}  which heuristic to use *)
let of_kind (module M : Mutations.MUTATOR) (module GD : VECDIST with type t = M.data) (module FD : VECDIST with type t = M.data) (module O : Oracle.ORACLE) = function
| ILS -> (module MkSearch (Iterated_local_search) (M) (GD) (FD) (O) : S)
| HC -> (module MkSearch (Hill_climbing) (M) (GD) (FD) (O) : S)
| RW -> (module MkSearch (Random_walk) (M) (GD) (FD) (O) : S)
| SA -> (module MkSearch (Simulated_annealing) (M) (GD) (FD) (O) : S)
| MH -> (module MkSearch (Metropolis_hastings) (M) (GD) (FD) (O) : S)

(** Select from a string which heuristic to use **)
let of_string (module M : Mutations.MUTATOR) (module GD : VECDIST with type t = M.data) (module FD : VECDIST with type t = M.data) (module O : Oracle.ORACLE) s = 
    of_kind (module M) (module GD) (module FD) (module O) (kind_of_string s)