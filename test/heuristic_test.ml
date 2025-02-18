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

open OUnit2
open Xyntia_utils

let test_arith_dist _ =
    let module D = Distance.Arith in

    let x, y = Bitvector.of_int32 (Int32.of_int 5), Bitvector.of_int32 (Int32.of_int 2) in
    assert_equal 3.0 (D.dist x y);
    assert_equal 3.0 (D.dist y x)


let test_hamm_dist _ =
    let module D = Distance.Hamming in

    let x, y = Bitvector.of_int32 (Int32.of_int 0xffffffff), Bitvector.of_int32 (Int32.of_int 0xffffffff) in
    assert_equal 0.0 (D.dist x y);

    let x, y = Bitvector.of_int32 (Int32.of_int 0xffffffff), Bitvector.of_int32 (Int32.of_int 0x7fffffff) in
    assert_equal 1.0 (D.dist x y);

    let x, y = Bitvector.of_int32 (Int32.of_int 1000), Bitvector.of_int32 (Int32.of_int 459) in
    assert_equal 4.0 (D.dist x y);
    assert_equal 4.0 (D.dist y x)

let test_xor_dist _ =
    let module D = Distance.Xor in

    let x, y = Bitvector.of_int32 (Int32.of_int 5), Bitvector.of_int32 (Int32.of_int 2) in
    assert_equal 7.0 (D.dist x y);
    assert_equal 7.0 (D.dist y x)

let test_logarith_dist _ =
    let module D = Distance.Logarith in

    let x, y = Bitvector.of_int32 (Int32.of_int 5), Bitvector.of_int32 (Int32.of_int 4) in
    assert_equal 1.0 (D.dist x y);
    assert_equal 1.0 (D.dist y x)

let qcheck = [ 
    QCheck.Test.make ~count:1000 ~name:"positive_arith_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Arith in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            (D.dist bit1 bit2) >= 0.0);

    QCheck.Test.make ~count:1000 ~name:"min_diff_arith_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Arith in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            if Bitvector.equal bit1 bit2 then
                (D.dist bit1 bit2) = 0.0
            else
                not (D.is_zero (D.dist bit1 bit2)));

    QCheck.Test.make ~count:1000 ~name:"positive_hamming_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Hamming in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            (D.dist bit1 bit2) >= 0.0);

    QCheck.Test.make ~count:1000 ~name:"min_diff_hamming_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Hamming in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            if Bitvector.equal bit1 bit2 then
                (D.dist bit1 bit2) = 0.0
            else
                not (D.is_zero (D.dist bit1 bit2)));

    QCheck.Test.make ~count:1000 ~name:"positive_xor_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Xor in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            (D.dist bit1 bit2) >= 0.0);

    QCheck.Test.make ~count:1000 ~name:"min_diff_xor_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Xor in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            if Bitvector.equal bit1 bit2 then
                (D.dist bit1 bit2) = 0.0
            else
                not (D.is_zero (D.dist bit1 bit2)));

    QCheck.Test.make ~count:1000 ~name:"positive_logarith_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Logarith in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            (D.dist bit1 bit2) >= 0.0);

    QCheck.Test.make ~count:1000 ~name:"min_diff_logarith_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Logarith in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            if Bitvector.equal bit1 bit2 then
                (D.dist bit1 bit2) = 0.0
            else
                not (D.is_zero (D.dist bit1 bit2)));

    QCheck.Test.make ~count:1000 ~name:"positive_syntia_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Syntia in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            (D.dist bit1 bit2) >= 0.0);

    QCheck.Test.make ~count:1000 ~name:"min_diff_syntia_dist"
        QCheck.(pair small_int small_int)
        (fun (i1, i2) ->  
            let module D = Distance.Syntia in
            let bit1, bit2 = (Bitvector.of_int32 (Int32.of_int i1)), (Bitvector.of_int32 (Int32.of_int i2)) in
            if Bitvector.equal bit1 bit2 then
                (D.dist bit1 bit2) = 0.0
            else
                not (D.is_zero (D.dist bit1 bit2)));
]

let suite =
    let ounit_suite = [
     "test_arith_dist">:: test_arith_dist;
     "test_hamming_dist">:: test_hamm_dist;
     "test_xor_dist">:: test_xor_dist;
     "test_logarith_dist">:: test_logarith_dist;
    ] in
    let qcheck_suite = List.map QCheck_ounit.to_ounit2_test qcheck in

    "suite">:::(List.append ounit_suite qcheck_suite)


let () =
  run_test_tt_main suite
