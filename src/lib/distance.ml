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

module Bv = Bitvector

let is_zero x = abs_float x < 0.01

module type DIST = sig
  val name : string
  val dist : Bv.t -> Bv.t -> float
  val is_zero : float -> bool
end

module type VECDIST = sig
  type t
  val name : string
  val vecdist : Bv.t array -> Bv.t array -> float
  val is_zero : float -> bool
  val extract : t Tree.t -> Bv.t array -> Bv.t array -> t Tree.t
end

(** Arithmetic distance **)
module Arith : DIST = struct
  let name = "arith"
  let dist x y = Bv.sub x y |> Bitvector.to_float |> Float.abs
  let is_zero = is_zero
end

(** Hamming distance **)
module Hamming : DIST = struct
  let name = "arith"
  let dist x y =
    let xored = Bv.logxor x y in
    let hamming = Bv.bit_count xored in
    Float.of_int hamming

  let is_zero = is_zero
end

(** Xor distance **)
module Xor : DIST = struct
  let name = "xor"
  let dist x y =
    let xored = Bitvector.logxor x y in
    Bitvector.to_float xored |> Float.abs

  let is_zero = is_zero
end

(** Logarithmetic distance **)
module Logarith : DIST = struct
  let name = "logarith"
  let dist x y =
    let arithme = Arith.dist x y in
    Float.log (arithme +. 1.) /. Float.log 2.

  let is_zero = is_zero
end

(** Distance used by Syntia in the paper "Syntia: Synthesizing the Semantics of Obfuscated Code" by Blazytko et al. **)
module Syntia : DIST = struct
  let name = "syntia"
  let dist_arith x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let n = Arith.dist x y in
    let divisor = (2. ** fl_sz) -. 1. in
    n /. divisor

  let dist_hamm x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let hamming = Hamming.dist x y in
    hamming /. fl_sz

  let trailing_ones_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let lz_x = Bv.trailing_ones x |> Float.of_int in
    let lz_y = Bv.trailing_ones y |> Float.of_int in
    Float.abs (lz_x -. lz_y) /. fl_sz

  let trailing_zeros_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let lz_x = Bv.trailing_zeros x |> Float.of_int in
    let lz_y = Bv.trailing_zeros y |> Float.of_int in
    Float.abs (lz_x -. lz_y) /. fl_sz

  let leading_ones_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let fo_x = Bv.leading_ones x |> Float.of_int in
    let fo_y = Bv.leading_ones y |> Float.of_int in
    Float.abs (fo_x -. fo_y) /. fl_sz

  let leading_zeros_dist x y =
    let fl_sz = Bv.size_of x |> Float.of_int in
    let fz_x = Bv.leading_zeros x |> Float.of_int in
    let fz_y = Bv.leading_zeros y |> Float.of_int in
    Float.abs (fz_x -. fz_y) /. fl_sz

  let dist x y =
    (dist_arith x y +. dist_hamm x y +. trailing_zeros_dist x y
   +. trailing_ones_dist x y +. leading_zeros_dist x y +. leading_ones_dist x y
    )
    /. 6.

  let is_zero x = abs_float x = 0.0
end

(** How do we aggregate the distance along a vector of input output *)
let dist_vec = Array.fold_left ( +. ) 0.

let vectorizeDist (type a) (dist_agg : float array -> float) (module D : DIST) (module T : Tree.TreeConstructor with type data = a) :
    (module VECDIST with type t = T.data) =
  let module M = struct
    type t = T.data
    let name = D.name
    let vecdist actual expected = Array.map2 D.dist actual expected |> dist_agg
    let extract tree _ _ = tree
    let is_zero = D.is_zero
  end in
  (module M)

(* TODO : For now, we use dist_vec, but we should parametrize
          the way we aggregate distance along the (output,expected) vector
*)
module GuideModCst (D : DIST) (O : Oracle.ORACLE) (M : Tree.TreeConstructor) = struct
  open Operators

  let always_0 vector = Array.fold_left Bv.logor vector.(0) vector
  let always_1 vector = Array.fold_left Bv.logand vector.(0) vector
  let always_0_expected = always_0 (O.out_values ())
  let always_1_expected = always_1 (O.out_values ())


  let add_binop_cst_pattern op tree constant =
    if Binop.neutral_element op constant then 
      tree
    else
      M.mk_binop op tree (M.mk_const_of_bv constant)

  let add_pattern_offset = add_binop_cst_pattern Add

  let add_pattern_xor = add_binop_cst_pattern Xor

  let add_pattern_mask tree andmask ormask =
    let t1 = add_binop_cst_pattern And tree andmask in
    add_binop_cst_pattern Or t1 ormask

  
  let add_pattern_shiftl = add_binop_cst_pattern LShift
  let add_pattern_ushiftr = add_binop_cst_pattern RShiftu
  let add_pattern_scalar = add_binop_cst_pattern Mul
  let add_pattern_rotate = add_binop_cst_pattern RotateRight
  let add_pattern_linvar tree constant var =
    M.mk_binop Add tree (add_binop_cst_pattern Mul (M.mk_var var) constant)  

  module NoGuide = (val vectorizeDist dist_vec (module D) (module M))

  module Offset = struct
    type t = M.data
    let name = "offset-" ^ D.name
    let vecdist actual expected =
      let c = Bv.sub expected.(0) actual.(0) in
      let f act exp = D.dist exp (Bv.add act c) in
      Array.map2 f actual expected |> dist_vec

    let is_zero = D.is_zero

    let extract tree actual expected =
      let constant = Bv.sub expected.(0) actual.(0) in
      add_pattern_offset tree constant
  end

  module MinOffset = struct
    type t = M.data
    let name = "min-offset-" ^ D.name
    let vecdist actual expected =
      let consts = Array.map2 Bv.sub expected actual |> Array.to_list in
      let dist v =
        let m = Array.map (fun x -> Bv.add v x) actual in
        Array.map2 D.dist m expected |> dist_vec
      in
      let min = Utility.min_list dist consts in
      match min with
      | None -> Array.map2 D.dist actual expected |> dist_vec
      | Some (_,c) -> c

    let is_zero = D.is_zero
    let extract = Offset.extract
  end

  module Xor = struct
    type t = M.data
    let name = "xor-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let c = Bv.logxor expected.(0) actual.(0) in
      let f act exp = D.dist exp (Bv.logxor act c) in
      Array.map2 f actual expected |> dist_vec

    let extract tree actual expected =
      let constant = Bv.logxor expected.(0) actual.(0) in
      add_pattern_xor tree constant
  end

  module MinXor = struct
    type t = M.data
    let name = "min-xor-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let constants =
        Array.map2 Bv.logxor actual expected |> Array.to_list
      in

      let dist c =
        let xor_functor x = Bv.logxor c x in
        let new_actual = Array.map xor_functor actual in
        Array.map2 D.dist new_actual expected |> dist_vec
      in

      let min = Utility.min_list dist constants in
      match min with 
      | None -> Array.map2 D.dist actual expected |> dist_vec 
      | Some (_, c) -> c

    let extract = Xor.extract
  end

  module Mask = struct
    type t = M.data
    let name = "mask-" ^ D.name
    let is_zero = D.is_zero

    let andmask, ormask =
      let outputs = O.out_values () in
      let am = always_0 outputs in
      let om = always_1 outputs in
      (am, om)

    (** Mask the result **)
    let mask x = Bv.logor (Bv.logand x andmask) ormask

    let vecdist actual expected =
      let maskedoutputs = Array.map mask actual in
      Array.map2 D.dist maskedoutputs expected |> dist_vec

    let extract tree _ _ = add_pattern_mask tree andmask ormask
  end

  module MaskOTF = struct
    type masks = { andmask : Bv.t; ormask : Bv.t }

    type t = M.data
    let name = "maskotf-" ^ D.name
    let is_zero = D.is_zero

    let get_mask_otf actual =
      let andmask =
        Bv.logor always_0_expected (Bv.lognot (always_0 actual))
      in
      let ormask =
        Bv.logand always_1_expected (Bv.lognot (always_1 actual))
      in
      { andmask; ormask }

    let masked_outputs_otf actual =
      let { andmask = mask_and; ormask = mask_or } = get_mask_otf actual in
      let mask x = Bv.logor (Bv.logand x mask_and) mask_or in
      mask

    let vecdist actual expected =
      let mask_otf = masked_outputs_otf actual in
      let maskedoutputs = Array.map mask_otf actual in
      Array.map2 D.dist maskedoutputs expected |> dist_vec

    let extract tree actual _expected =
      let { andmask; ormask } = get_mask_otf actual in
      add_pattern_mask tree andmask ormask
  end

  module ShiftLeft = struct
    type t = M.data
    let name = "shiftleft-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let al0 = always_0 actual in
      let c = max (Bv.trailing_zeros always_0_expected - Bv.trailing_zeros al0) 0 in
      let f act exp = D.dist exp (Bv.shift_left act c) in
      Array.map2 f actual expected |> dist_vec

    let extract tree actual _expected =
      let al0 = always_0 actual in
      let c = max (Bv.trailing_zeros always_0_expected - Bv.trailing_zeros al0) 0 in
      let bc = Bv.of_int ~size:(Bv.size_of actual.(0)) c in
      add_pattern_shiftl tree bc
  end

  module MinShiftLeft = struct
    type t = M.data
    let name = "min-shiftleft-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let constants = List.init (Bv.size_of actual.(0)) Fun.id in

      let dist c =
        let shiftleft_functor x = Bv.shift_left x c in
        let new_actual = Array.map shiftleft_functor actual in
        Array.map2 D.dist new_actual expected |> dist_vec
      in

      let min = Utility.min_list dist constants in
      match min with 
      | None -> Array.map2 D.dist actual expected |> dist_vec
      | Some (_, c) -> c

    let extract = ShiftLeft.extract
  end

  module UShiftRight = struct
    type t = M.data
    let name = "ushiftright-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let al0 = always_0 actual in
      let c = max (Bv.leading_zeros always_0_expected - Bv.leading_zeros al0) 0 in
      let f act exp = D.dist exp (Bv.shift_right act c) in
      Array.map2 f actual expected |> dist_vec

    let extract tree actual _expected =
      let al0 = always_0 actual in
      let c = max (Bv.leading_zeros always_0_expected - Bv.leading_zeros al0) 0 in
      let bc = Bv.of_int ~size:(Bv.size_of actual.(0)) c in
      add_pattern_ushiftr tree bc
  end

  module MinUShiftRight = struct
    type t = M.data
    let name = "min-ushiftright-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let constants = List.init (Bv.size_of actual.(0)) Fun.id in

      let dist c =
        let shiftright_functor x = Bv.shift_right x c in
        let new_actual = Array.map shiftright_functor actual in
        Array.map2 D.dist new_actual expected |> dist_vec
      in

      let min = Utility.min_list dist constants in
      match min with 
      | None -> Array.map2 D.dist actual expected |> dist_vec 
      | Some (_, c) -> c

    let extract = UShiftRight.extract
  end

  let inverse_odd x e_star = Bv.mul (Bitvector.mod_inverse x) e_star

  module Mul = struct
    type t = M.data
    let name = "mul-" ^ D.name
    let is_zero = D.is_zero

    let get_scalar_constant actual expected =
      let rec find_odd x y =
        match (x, y) with
        | h_a :: t_a, h_e :: t_e ->
            if not (Bitvector.is_even h_a) then Some (h_a, h_e)
            else find_odd t_a t_e
        | [], [] -> None
        | _ -> failwith "Should not happen"
      in
      match find_odd (Array.to_list actual) (Array.to_list expected) with
      | None -> Bv.ones (Bv.size_of expected.(0))
      | Some (e_x, e_star_x) -> inverse_odd e_x e_star_x

    let vecdist actual expected =
      let c = get_scalar_constant actual expected in
      let f a = Bv.mul a c in
      let new_output = Array.map f actual in
      Array.map2 D.dist new_output expected |> dist_vec

    let extract tree actual expected =
      let c = get_scalar_constant actual expected in
      add_pattern_scalar tree c
  end

  module MinMul = struct
    type t = M.data
    let name = "min-mul-" ^ D.name
    let is_zero = D.is_zero

    let min_get_scalar_constant actual expected =
      (*
          1. Iterate through actual/expected
          2. Keep constant that minimizes the distance
          3. Return 
      *)
      let sz = Bv.size_of actual.(0) in

      let find_constant al el =
        let neutral_cost = Array.map2 D.dist actual expected |> dist_vec in

        List.fold_left2 (fun (cst, cost) h_a h_e ->
          if Bitvector.is_even h_a then (cst, cost)
          else
            let c = inverse_odd h_a h_e in
            let new_actual = Array.map (Bv.mul c) actual in
            let d = Array.map2 D.dist new_actual expected |> dist_vec in
            if d < cost then (c, d) else (cst, cost)
        ) (Bv.ones sz, neutral_cost) al el
      in
      find_constant (Array.to_list actual) (Array.to_list expected)

    let vecdist actual expected =
      if actual = expected then 0.0
      else
        let (_, cost) = min_get_scalar_constant actual expected in
        cost

    let extract tree actual expected = Mul.extract tree actual expected
  end

  let find_rotations bva bve =
    let sz = Bv.size_of bva in
    List.fold_left
      (fun acc i ->
        if Bv.rotate_right bva i = bve then i :: acc else acc)
      [] (List.init sz Fun.id)

  let try_all_rotations a e d =
    List.for_all2
      (fun ha he -> Bv.equal (Bv.rotate_right ha d) he)
      a e

  let try_rotation a e d_list = List.find_opt (try_all_rotations a e) d_list

  module Rotate = struct
    type t = M.data
    let name = "rotate-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let new_ror d a = Bv.rotate_right a d in
      let sz = Bv.size_of actual.(0) in

      let cost_rot i =
        let new_actual = Array.map (new_ror i) actual in
        Array.map2 D.dist new_actual expected |> dist_vec
      in

      if true then
        let min = Utility.min_list cost_rot (List.init sz Fun.id) in
        match min with None -> failwith "should not happen" | Some (_, d) -> d
      else
        (* TODO : Why use last_index ? *)
        let last_index = Array.length actual - 1 in
        let d = find_rotations actual.(last_index) expected.(last_index) in

        let ok_rot =
          try_rotation (Array.to_list actual) (Array.to_list expected) d
        in
        match ok_rot with
        | None -> Array.map2 D.dist actual expected |> dist_vec
        | Some rotation_constant ->
            let f a = Bv.rotate_right a rotation_constant in
            let new_output = Array.map f actual in
            Array.map2 D.dist new_output expected |> dist_vec

    let get_rotation_final actual expected =
      (* TODO : Why use last_index ? *)
      let last_index = Array.length actual - 1 in
      let d = find_rotations actual.(last_index) expected.(last_index) in
      let ok_rot =
        try_rotation (Array.to_list actual) (Array.to_list expected) d
      in
      Option.value ~default:0 ok_rot

    let extract tree actual expected =
      let c =
        get_rotation_final actual expected
        |> Bv.of_int ~size:(Bv.size_of actual.(0))
      in
      add_pattern_rotate tree c
  end

  let odd_even_factor x : Bv.t * int =
    if Bv.is_zeros x then
      (Bv.ones (Bv.size_of x), Bv.size_of x)
    else
      let n = Bv.trailing_zeros x in 
      (Bv.shift_right x n, n)

  let find_factor x goal : Bv.t option = 
    (* We can only infer a factor if x is odd or 
       if x and goal are even. *)
    if Bitvector.is_even x then
      if not (Bitvector.is_even goal) then
        None
      else
        let (px, nx) = odd_even_factor x in
        let (pg, ng) = odd_even_factor goal in
        if nx > ng then
          None
        else
          Bv.shift_left (Bv.ones (Bv.size_of x)) (ng - nx)
          |> Bv.mul pg
          |> Bv.mul (Bitvector.mod_inverse px)
          |> Option.some
    else
      Some (inverse_odd x goal)
  
  module LinearComb () = struct
    (* TODO : /!\ May be slightly off if the expression is already a polynomial *)
    module SMap = Map.Make (String)
    module IMap = Map.Make (Int)

    type input_row_type = 
    | Zeros
    | SingleVar of Oracle.variable
    | FullOne
    | Other

    type t = M.data
    let name = "linearcomb-" ^ D.name

    let classify_sample sample =
      Array.fold_left (fun acc (var, bv) ->
        match acc, Bv.is_zeros bv, Bv.is_ones bv with
        | Zeros, true, _ -> acc
        | Zeros, false, true -> SingleVar var
        | SingleVar _, true, _ -> acc
        | SingleVar _, false, true -> FullOne
        | FullOne, false, true -> FullOne
        | FullOne, true, _ -> Other
        | _, false, false -> Other
        | Other, _, _ -> Other
        ) Zeros
        (O.sample_inputs sample)

    (* Check that input 1 is (0,...,0) 
         and input i contains only 1 non-zero element
         equal to 1 and for the variable i-1,
         for i in [1; n] *)
    let search_inputs () =
      let nsamples = O.nsamples () in
      let var_map = ref SMap.empty in
      let zero_sample = ref None in
      let fullone_sample = ref None in
      let other_samples = ref [] in

      let rec loop i =
        if i >= nsamples then ()
        else
          let sample = O.get_sample i in
          (match classify_sample sample with
          | Zeros -> zero_sample := Some i
          | SingleVar v -> var_map := SMap.update v.name (fun _ -> Some i) !var_map
          | FullOne -> fullone_sample := Some i
          | Other -> other_samples := i :: !other_samples);
          loop (i+1)
      in
      loop 0;
      assert (Option.is_some !zero_sample);
      assert (Option.is_some !fullone_sample);
      assert (List.length !other_samples > 0);
      Array.iter (fun (v : Oracle.variable) -> 
        assert (Option.is_some (SMap.find_opt v.name !var_map))
      ) (O.vars ());
      Option.get !zero_sample, !var_map, Option.get !fullone_sample, List.rev !other_samples
    
    let zero_sample, var_map, fullone_sample, other_samples = search_inputs ()

    (* Pre-compute a shared part of the alpha computation for all possible other_samples *)
    let vecfactor vec v0 v1 =
      let bv_size = Bv.size_of v0 in
      let acc = List.fold_left (fun acc (xi, oi) -> 
        Bv.add acc (Bv.mul xi (Bv.sub v0 oi))) 
        (Bv.zeros bv_size) vec in
      Bv.add acc (Bv.sub v1 v0)

    let compute_shared_alpha_part () =
      let outs = O.out_values () in
      let v0 = outs.(zero_sample) in
      let vn = SMap.map (
        fun i -> (i, outs.(i))
      ) var_map 
      |> SMap.bindings
      |> List.map snd
      in
      let alphar = ref IMap.empty in
      List.iter (fun i -> 
        let s = O.get_sample i in
        let (_,v1) = O.sample_output s in
        let vouts = O.sample_inputs s in
        let vec = List.map (fun (j, oi) -> snd (vouts.(j)), oi) vn in
        alphar := IMap.add i (vecfactor vec v0 v1) !alphar
      ) (fullone_sample :: other_samples);
      !alphar

    let alphar = compute_shared_alpha_part ()

    (* TODO : Correct the formula *)
    let compute_alpha actual _ =
      let rec loop_find_output poss = function
      | i :: t -> (
        let right = IMap.find i alphar in
        let sample = O.get_sample i in
        let ins = O.sample_inputs sample in
        let v0 = actual.(zero_sample) and
            v1 = actual.(i) and
            vn = SMap.map (
              fun j -> (snd ins.(j), actual.(j))
            ) var_map 
            |> SMap.bindings
            |> List.map snd
        in
        let left = vecfactor vn v0 v1 in
        match find_factor left right, poss with
        | None, None -> loop_find_output poss t
        | Some alpha, _ when not (Bitvector.is_even alpha) -> alpha
        | (Some _ as alpha), None
        | None, (Some _ as alpha) -> loop_find_output alpha t
        | Some alpha, Some prev_alpha -> 
          let (_, nal) = odd_even_factor alpha in
          let (_, npa) = odd_even_factor prev_alpha in
          if nal < npa then loop_find_output (Some alpha) t
          else loop_find_output poss t
        )
      | [] -> 
        Option.value ~default:(Bv.ones (Bv.size_of actual.(0))) poss
      in
      loop_find_output None (fullone_sample :: other_samples)
    
    let compute_beta actual expected alpha =
      Bv.sub expected.(zero_sample) (Bv.mul alpha actual.(zero_sample))

    let compute_vcoefs actual expected alpha beta =
      SMap.map (fun i ->
        Bv.sub (Bv.sub expected.(i) (Bv.mul alpha (actual.(i)))) beta
      ) var_map

    let is_zero = D.is_zero

    let vecdist actual expected =
      let vars = O.vars () in
      let alpha = compute_alpha actual expected in
      let beta = compute_beta actual expected alpha in
      let coefs = compute_vcoefs actual expected alpha beta in
      let arr = Array.map (fun v -> 
        let output = O.var_values v in
        let coef = SMap.find v.name coefs in
        Array.map (fun x -> Bv.mul x coef) output
      ) vars
      in
      let nactual = Array.init (Array.length actual) (fun i -> 
        let bv = Array.fold_left (fun acc out -> Bv.add acc out.(i)) 
                                 (Bv.zeros (Bv.size_of actual.(0))) arr
        in
        Bv.add (Bv.mul alpha actual.(i)) (Bv.add bv beta)
      ) in
      Array.map2 (D.dist) nactual expected |> dist_vec

    let extract tree actual expected =
      let vars = O.vars () in
      let alpha = compute_alpha actual expected in
      let beta = compute_beta actual expected alpha in
      let coefs = compute_vcoefs actual expected alpha beta in
      let tree = add_pattern_offset (add_pattern_scalar tree alpha) beta
      in
      Array.fold_left (fun t (var : Oracle.variable) ->
        let coef = SMap.find var.name coefs in
        add_pattern_linvar t coef var
      ) tree vars
  end


  let find_affine_coefficients actual expected =
    let o1 = actual.(0) in
    let len = Array.length actual in
    let rec loop_find i pos =
      if i >= len then pos
      else (
        if Bitvector.is_even (Bv.sub o1 actual.(i)) then (
          if Bv.trailing_zeros (Bv.sub o1 actual.(i)) > 
             Bv.trailing_zeros (Bv.sub expected.(0) expected.(i)) then 
            loop_find (i+1) pos
          else 
            loop_find (i+1) (Some i)
        )
        else
          Some i
      )
    in
    match loop_find 1 None with
    | None -> 
      (* No method to find the coef, we fallback on a simple offset *)
      (Bv.ones (Bv.size_of o1), Bv.sub expected.(0) actual.(0))
    | Some i ->
      let o2 = actual.(i) in
      let alpha = find_factor (Bv.sub o1 o2) (Bv.sub expected.(0) expected.(i)) |> Option.get in
      let beta = Bv.sub expected.(i) (Bv.mul alpha o2) in
      (alpha, beta)

  module Affine = struct
    type t = M.data
    let name = "affine-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let (alpha, beta) = find_affine_coefficients actual expected in
      let f act exp = D.dist exp (Bv.add (Bv.mul act alpha) beta) in
      Array.map2 f actual expected |> dist_vec

    let extract tree actual expected =
      let (alpha, beta) = find_affine_coefficients actual expected in
      add_pattern_offset (add_pattern_scalar tree alpha) beta
  end

  module Byteswap = struct
    type t = M.data
    let name = "byteswap-" ^ D.name
    let is_zero = D.is_zero

    let vecdist actual expected =
      let d1 = Array.map2 D.dist actual expected |> dist_vec in
      let f act exp = D.dist exp (Bitvector.byteswap act) in
      let d2 = Array.map2 f actual expected |> dist_vec in
      min d1 d2

    let extract tree actual expected =
      let d1 = Array.map2 D.dist actual expected |> dist_vec in
      if D.is_zero d1 then
        tree
      else
        M.mk_unop Byteswap tree

  end

  module Poly2 = struct
    type io_triple = {
      mutable io1: int option;
      mutable io2: int option;
      mutable io3: int option
    }

    type t = M.data
    let name = "poly2-" ^ D.name
    let check_io_triple = function
      | { io1=Some _; io2=Some _ ; io3 =Some _} -> true
      | _ -> false

    let scan_inputs (actual: Bv.t array) (expected: Bv.t array) =
      let size = Array.length actual in
      (*
        [e/o]i[e/o]o : [even/odd]input[even/odd]output
      *)
      let eieo = { io1=None; io2=None; io3=None } and 
          eioo = { io1=None; io2=None; io3=None } and 
          oieo = { io1=None; io2=None; io3=None } and 
          oioo = { io1=None; io2=None; io3=None } in
      let rec loop i =
        if i >= size then None
        else (
          let x = actual.(i) and y = expected.(i) in
          (
            if Bitvector.is_even x then (
              eieo.io1 <- Some (Option.value eieo.io1 ~default:i);
              eioo.io1 <- Some (Option.value eioo.io1 ~default:i);
            )
            else (
              oieo.io1 <- Some (Option.value oieo.io1 ~default:i);
              oioo.io1 <- Some (Option.value oioo.io1 ~default:i);
            )
          );
          (match Bv.get_bit x 0, Bv.get_bit x 1, Bv.get_bit y 0 with
          | false, false, false -> 
            (* x = 0 [4]; y = 0 [2] *)
            oieo.io2 <- Some (Option.value oieo.io2 ~default:i)
          | false, false, true  -> 
            (* x = 0 [4]; y = 1 [2] *)
            oioo.io2 <- Some (Option.value oioo.io2 ~default:i)
          | false, true, false  -> 
            (* x = 2 [4]; y = 0 [2] *)
            oieo.io3 <- Some (Option.value oieo.io3 ~default:i)
          | false, true, true   -> 
            (* x = 2 [4]; y = 1 [2] *)
            oioo.io3 <- Some (Option.value oioo.io3 ~default:i)
          | true, false, false  -> 
            (* x = 1 [4]; y = 0 [2] *)
            eieo.io2 <- Some (Option.value eieo.io2 ~default:i)
          | true, false, true   -> 
            (* x = 1 [4]; y = 1 [2] *)
            eioo.io2 <- Some (Option.value eioo.io2 ~default:i)
          | true, true, false   -> 
            (* x = 3 [4]; y = 0 [2] *)
            eieo.io3 <- Some (Option.value eieo.io3 ~default:i)
          | true, true, true    -> 
            (* x = 3 [4]; y = 1 [2] *)
            eioo.io3 <- Some (Option.value eioo.io3 ~default:i)
          );
          match List.find_opt check_io_triple [eieo; eioo; oieo; oioo] with
          | Some t -> Some t
          | _ -> loop (i+1)
        )
      in
      loop 0

      let is_zero = D.is_zero

      let get_coefs actual expected =
        match scan_inputs actual expected with
        | Some { io1=(Some i1); io2=(Some i2); io3=(Some i3) } -> (
          let x1 = actual.(i1) and y1 = expected.(i1) in
          let x2 = actual.(i2) and y2 = expected.(i2) in
          let x3 = actual.(i3) and y3 = expected.(i3) in
          
          let di31 = Bv.sub x3 x1 in
          let di21 = Bv.sub x2 x1 in 
          let di32 = Bv.sub x3 x2 in 
          let do31 = Bv.sub y3 y1 in 
          let do21 = Bv.sub y2 y1 in

          if (Bitvector.is_even di21) || (Bitvector.is_even di31) then
            Format.printf "ERROR : x1=%a; x2=%a; x3=%a" Bv.pp x1 Bv.pp x2 Bv.pp x3;
          assert (not (Bitvector.is_even di21));
          assert (not (Bitvector.is_even di31));
          let inv21 = Bitvector.mod_inverse di21 in
          let inv31 = Bitvector.mod_inverse di31 in

          let lambda21 = Bv.mul do21 inv21 in
          let lambda = Bv.sub (Bv.mul do31 inv31) lambda21 in

          (* Due to how we selected the inputs, we have : 
              - di32 = 2*c with c odd
              - lambda = 2*d with d of unknown parity
            thus we can easily build a pseudo-inverse
          *)
          assert (Bitvector.is_even lambda);
          assert (Bitvector.is_even di32);
          let odd32 = Bv.shift_right di32 1 in
          let inv32 = Bitvector.mod_inverse odd32 in 
          let lambdadiv = Bv.shift_right lambda 1 in
          let a2 = Bv.mul lambdadiv inv32 in
          let a1 = Bv.sub lambda21 (Bv.mul a2 (Bv.add x1 x2)) in
          let a0 = Bv.sub y1 (Bv.mul a1 x1) in
          let a0 = Bv.sub a0 (Bv.mul a2 (Bv.mul x1 x1)) in
          (a2, a1, a0)
        )
        | _ ->
          (* In this case, we fix a_2 at 1 and solve the remaining *)
          let f x y = Bv.sub y (Bv.mul x x) in
          let expected = Array.map2 f actual expected in
          let (a1, a0) = find_affine_coefficients actual expected in
          (Bv.ones (Bv.size_of actual.(0)), a1, a0)

      let vecdist actual expected = 
        let (a2, a1, a0) = get_coefs actual expected in
        let f act exp = D.dist exp Bv.(add (mul a2 (mul act act)) (add (mul act a1) a0)) in
        Array.map2 f actual expected |> dist_vec
      
      let extract tree actual expected =
        let (a2, a1, a0) = get_coefs actual expected in
        let f act exp = D.dist exp Bv.(add (mul a2 (mul act act)) (add (mul act a1) a0)) in
        let ds = Array.map2 f actual expected |> dist_vec in
        assert (is_zero ds);
        M.mk_binop Mul (M.mk_const_of_bv a2) (M.mk_binop Mul tree tree)
        |> M.mk_binop Add (M.mk_binop Mul (M.mk_const_of_bv a1) tree)
        |> M.mk_binop Add (M.mk_const_of_bv a0)
  end
end

module CombineVecDist (Vs : sig
  type t
  val combinator : float -> float -> float
  val dists : (module VECDIST with type t = t) list
end) =
struct
  type t = Vs.t
  module type VD = VECDIST with type t = t
  let name = List.fold_left (fun acc (module V : VD) -> 
    if String.equal acc String.empty then V.name
    else acc ^ "-" ^ V.name) "" Vs.dists

  let vecdist actual expected =
    match Vs.dists with
    | [] -> failwith "Combining an empty list of vector distances"
    | (module V1 : VD) :: t ->
        List.fold_left
          (fun acc (module V : VD) ->
            Vs.combinator acc (V.vecdist actual expected))
          (V1.vecdist actual expected)
          t

  let extract tree actual expected =
    let (module V : VD) =
      List.find
        (fun (module V : VD) -> V.is_zero (V.vecdist actual expected))
        Vs.dists
    in
    V.extract tree actual expected

  let is_zero x = List.exists (fun (module V : VD) -> V.is_zero x) Vs.dists
end


let infrules_of_str (type a) str (module D : DIST) (module O : Oracle.ORACLE) (module T : Tree.TreeConstructor with type data = a) : (module VECDIST with type t = a) list = 
  let module GMC = GuideModCst (D) (O) (T) in
  let aux : string -> (module VECDIST with type t = a) = function
  | "+" -> (module GMC.MinOffset)
  | "+_l" -> (module GMC.Offset)
  | "*" -> (module GMC.MinMul)
  | "*_l" -> (module GMC.Mul)
  | "mask" -> (module GMC.Mask)
  | "maskotf" -> (module GMC.MaskOTF)
  | "^" -> (module GMC.MinXor)
  | "^_l" -> (module GMC.Xor)
  | ">>u" -> (module GMC.MinUShiftRight)
  | ">>u_l" -> (module GMC.UShiftRight)
  | "<<" -> (module GMC.MinShiftLeft)
  | "<<_l" -> (module GMC.ShiftLeft)
  | "ror" -> (module GMC.Rotate)
  | "affine" -> (module GMC.Affine)
  | "poly2" -> (module GMC.Poly2)
  | _ -> failwith (Format.sprintf "Unkown inference rule: '%s'" str)
  in
  match str with
  | "mba" -> List.map aux ["+"; "maskotf"; "<<"; "*"; "^"]
  | "all" -> List.map aux ["+"; "maskotf"; "<<"; ">>u"; "*"; "^"; "ror"; "poly2"; "affine"]
  | _ -> [aux str]