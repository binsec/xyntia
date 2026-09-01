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

[@@@warning "-32"] (* Ignore the warning of unused function for the TS module *)
open Xyntia_utils
open OUnit2

module T = Tree.UnitTreeConstructor
module S = Simplifier.TreeSimplifier (T)

module TS = struct
  include T
  include Treeshortcut.Make (struct 
    type t = unit Tree.t
    include T
  end)
end

let suite =
  let (module O) = Oracle.dummyOracle in
  let module S = S (O) in

  (* Setup variable and constants *)
  let vars = Array.map TS.mk_var (O.vars ()) in
  let x0 = vars.(0) in
  let x1 = vars.(1) in
  let x2 = vars.(2) in
  let x3 = vars.(3) in
  let x4 = vars.(4) in
  let zero = TS.mk_const_of_bv (Bitvector.zeros 32) in
  let one = TS.mk_const_of_bv (Bitvector.ones 32) in
  let minusone = TS.mk_const_of_bv (Bitvector.max_ubv 32) in
  let minustwo = TS.mk_const_of_bv (Bitvector.of_int ~size:32 (-2)) in
  let two = TS.mk_const_of_bv (Bitvector.of_int ~size:32 2) in
  let four = TS.mk_const_of_bv (Bitvector.of_int ~size:32 4) in
  let six = TS.mk_const_of_bv (Bitvector.of_int ~size:32 6) in
  let sminbv = TS.mk_const_of_bv (Bitvector.min_sbv 32) in
  let smaxbv = TS.mk_const_of_bv (Bitvector.max_sbv 32) in

  let simp_test tested expected _ctx =
    (* Test that it works *)
    assert_equal 
      ~cmp:Tree.tree_equal
      ~printer:(Tree.to_string (fun () -> ""))
      expected (S.simplify tested)
  in
  let (-->) = simp_test in

  let ounit_suite = [
    (* Involutions *)
    "involution_neg">:: TS.(~. (~. x0)) --> x0;
    "involution_not">:: TS.(- (- x0)) --> x0;

    (* Nilpotence *)
    "nilpotent_xor">:: TS.(x0 ^ x0) --> zero;
    "nilpotent_sub">:: TS.(x0 - x0) --> zero;

    (* Idempotence *)
    "idempotence_and">:: TS.(x0 && x0) --> x0;
    "idempotence_or">::  TS.(x0 || x0) --> x0;
    "idempotence_umax">:: TS.(umax x0 x0) --> x0;
    "idempotence_umin">:: TS.(umin x0 x0) --> x0;
    "idempotence_smax">:: TS.(smax x0 x0) --> x0;
    "idempotence_smin">:: TS.(smin x0 x0) --> x0;

    (* Neutral elements *)
    "neutral_add_right">:: TS.(x0 + zero) --> x0;
    "neutral_add_left">:: TS.(zero + x0) --> x0;

    "neutral_sub">:: TS.(x0 - zero) --> x0;
    
    "neutral_mul_right">:: TS.(x0 * one) --> x0;
    "neutral_mul_left">:: TS.(one * x0) --> x0;
    
    "neutral_and_right">:: TS.(x0 && minusone) --> x0;
    "neutral_and_left">:: TS.(minusone && x0) --> x0;
    
    "neutral_or_right">:: TS.(x0 || zero) --> x0;
    "neutral_or_left">:: TS.(zero || x0) --> x0;
    
    "neutral_xor_right">:: TS.(x0 ^ zero) --> x0;
    "neutral_xor_left">:: TS.(zero ^ x0) --> x0;
    
    "neutral_rshiftu_left">:: TS.(x0 >> zero) --> x0;
    "neutral_rshifts_left">:: TS.(x0 >>- zero) --> x0;
    "neutral_lshift_left">:: TS.(x0 << zero) --> x0;
    "neutral_rshiftux86_left">:: TS.(x0 >>% zero) --> x0;
    "neutral_rshiftsx86_left">:: TS.(x0 >>-% zero) --> x0;
    "neutral_lshiftx86_left">:: TS.(x0 <<% zero) --> x0;

    "neutral_umax_right">:: TS.(umax x0 zero) --> x0;
    "neutral_umax_left">:: TS.(umax zero x0) --> x0;

    "neutral_umin_right">:: TS.(umin x0 minusone) --> x0;
    "neutral_umin_left">:: TS.(umin minusone x0) --> x0;

    (* Combination of AC + idempotent *)
    "ac_idem_and1">:: TS.(( x0 && x1 ) && x0) --> TS.( x0 && x1 );
    "ac_idem_and2">:: TS.(( x1 && x0 ) && x0) --> TS.( x1 && x0 );
    "ac_idem_and3">:: TS.( x0 && ( x0 && x1 )) --> TS.( x0 && x1 );
    "ac_idem_and4">:: TS.( x0 && ( x1 && x0 )) --> TS.( x1 && x0 );

    (* Combination of AC + nilpotent *)
    "ac_nil_xor1">:: TS.(( x0 ^ x1 ) ^ x0) --> x1;
    "ac_nil_xor2">:: TS.(( x1 ^ x0 ) ^ x0) --> x1;
    "ac_nil_xor3">:: TS.( x0 ^ ( x0 ^ x1 )) --> x1;
    "ac_nil_xor4">:: TS.( x0 ^ ( x1 ^ x0 )) --> x1;

    "ac_nil_xor5">:: TS.( (x0 ^ x1) ^ ( x2 ^ x0 )) --> TS.(x1 ^ x2);
    "ac_nil_xor6">:: TS.( (x1 ^ x0) ^ ( x2 ^ x0 )) --> TS.(x1 ^ x2);

    (* (x && y) && (x && z) -> x && (y && z) *)
    "ac_idem_and5">:: TS.(( x0 && x1 ) && ( x0 && x2 )) --> TS.( x0 && (x1 && x2) );
    "ac_idem_and6">:: TS.(( x0 && x1 ) && ( x2 && x0 )) --> TS.( x0 && (x1 && x2) );
    "ac_idem_and7">:: TS.(( x1 && x0 ) && ( x0 && x2 )) --> TS.( x0 && (x1 && x2) );
    "ac_idem_and8">:: TS.(( x1 && x0 ) && ( x2 && x0 )) --> TS.( x0 && (x1 && x2) );
    (* Hard AC + idem case *)
    "ac_idem_and9">:: TS.(( x0 && x1 ) && ( x3 && (x4 && x0) )) --> TS.( ((x4 && x3) && x1) && x0 );
    "ac_idem_and10">:: TS.(( x0 && four ) && ( x3 && (x4 && six) )) --> TS.( ((four && x4) && x3) && x0) ;

    (* Absorbing elements *)
    "abs_or1">:: TS.(x0 || minusone) --> minusone;
    "abs_or2">:: TS.(minusone || x0) --> minusone;

    "abs_and1">:: TS.(x0 && zero) --> zero;
    "abs_and2">:: TS.(zero && x0) --> zero;

    "abs_mul1">:: TS.(x0 * zero) --> zero;
    "abs_mul2">:: TS.(zero * x0) --> zero;

    "abs_umin1">:: TS.(umin x0 zero) --> zero;
    "abs_umin2">:: TS.(umin zero x0) --> zero;

    "abs_umax1">:: TS.(umax x0 minusone) --> minusone;
    "abs_umax2">:: TS.(umax minusone x0) --> minusone;

    "abs_smin1">:: TS.(smin x0 sminbv) --> sminbv;
    "abs_smin2">:: TS.(smin sminbv x0) --> sminbv;

    "abs_smax1">:: TS.(smax x0 smaxbv) --> smaxbv;
    "abs_smax2">:: TS.(smax smaxbv x0) --> smaxbv;

    (* Specialized opt *)
    "add_xor_and1">:: TS.( (x0 ^ x1) + (x0 && x1) ) --> TS.(x0 || x1);
    "add_xor_and2">:: TS.( (x1 ^ x0) + (x0 && x1) ) --> TS.(x0 || x1);

    "add_sub1">:: TS.( (x0 - x1) + x1 ) --> x0;
    "add_sub2">:: TS.( x1 + (x0 - x1) ) --> x0;

    (* (- a) + b = b - a *)
    "add_minus1">:: TS.(- x0 + x1) --> TS.( x1 - x0 );
    "add_minus2">:: TS.( x1 + (- x0) ) --> TS.( x1 - x0 );

    (* ~a + a = -1 *)
    "add_not1">:: TS.( (~. x0) + x0 ) --> minusone;
    "add_not2">:: TS.( x0 + (~. x0) ) --> minusone;

    (* ~a + 1 = -a *)
    "add_not3">:: TS.( (~. x0) + one ) --> TS.( - x0);
    "add_not4">:: TS.( one + (~. x0) ) --> TS.( - x0);
         
    (* ~a + (a + b) = b - 1 *)
    "add_not_add1">:: TS.( (~. x0) + (x0 + x1) ) --> TS.( x1 - one );
    "add_not_add2">:: TS.( (~. x0) + (x1 + x0) ) --> TS.( x1 - one );
    "add_not_add3">:: TS.( (x1 + x0) + (~. x0) ) --> TS.( x1 - one );
    "add_not_add4">:: TS.( (x0 + x1) + (~. x0) ) --> TS.( x1 - one );

    (* a + a = 2*a *)
    "add_var">:: TS.( x0 + x0 ) --> TS.( two * x0 );

    (* -a * -b = a * b *)
    "mul_uminus_uminus">:: TS.( (-x0) * (-x1)) --> TS.(x0 * x1);

    (* -a * b = - (a * b) *)
    "mul_uminus1">:: TS.( (-x0) * x1) --> TS.(- (x0 * x1));
    "mul_uminus2">:: TS.( x0 * (-x1)) --> TS.(- (x0 * x1));

    (* -a - 1 = ~a *)
    "sub_minus1">:: TS.( (- x0) - one ) --> TS.( ~. x0 );
      
      (* a - (-b) = a + b *)
    "sub_minus2">:: TS.( x0 - ( -x1 ) ) --> TS.( x0 + x1 );
      
      (* -a - b = - (a + b) *)
    "sub_minus3">:: TS.( (- x0) - x1 ) --> TS.( - ( x0 + x1 ) );

    (* (a ^ b) & (a & b) = 0 *)
    "and_xor_and">:: TS.( (x0 ^ x1) && ( x0 && x1 ) ) --> zero;

    (* a & ~a  = 0 *)
    "and_not">:: TS.( x0 && ( ~. x0 ) ) --> zero;

    (* (a & b) & ~a = 0 *)
    "and_and_not">:: TS.( (x0 && x1) && ( ~. x0 ) ) --> zero;

    (* a & (~a & b) = 0 *)
    "and_and_not2">:: TS.( x0 && ( (~. x0) && x1) ) --> zero;

    (* (a ^ b) | a = a | b *)
    "or_xor">:: TS.( (x0 ^ x1) || x0 ) --> TS.( x0 || x1 );

    (* a | ~a = -1 *)
    "or_not">:: TS.( x0 || (~. x0) ) --> minusone;

    (* (a & b) | a = a *)
    "or_and">:: TS.( ( x0 && x1 ) || x0 ) --> x0;

    (* (a | b) | ~a = -1 *)
    "or_or_not">:: TS.( ( x0 || x1 ) || ( ~. x0 ) ) --> minusone;

    (* a ^ ~a = -1 *)
    "xor_not">:: TS.( x0 ^ ( ~. x0 ) ) --> minusone;

    (* (-(1 | a)) ^ (1 | a) = -2 *)
    "xor_spe">:: TS.( (- (one || x0)) ^ (one || x0) ) --> minustwo;

    (* Test with nested AC operators *)
    "nested_ac">:: TS.( x0 && (x1 && (x0 && (x2 - (x3 - (x1 ^ (x2 ^ (x1 ^ x4))))))) ) --> TS.(x0 && x1 && (x2 - (x3 - (x2 ^ x4))));
  ]
  in

  "suite">:::(ounit_suite)


let () =
  run_test_tt_main suite
