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

let test_size_of _ =
    let b = Bitvector.of_int32 (Int32.of_int 5) in
    assert_equal 32 (Bitvector.size_of b)

let qcheck = [ 
    QCheck.Test.make ~count:1000 ~name:"test_sgt"
        QCheck.(pair int32 int32)
        (fun (i1, i2) ->  
            let b1, b2 = Bitvector.of_int32 i1, Bitvector.of_int32 i2 in
            (Bitvector.sgt b1 b2) = (i1 > i2)); 
]

let suite =
    let ounit_suite = [
     "test_sizeof">:: test_size_of;
    ] in
    let qcheck_suite = List.map QCheck_ounit.to_ounit2_test qcheck in

    "suite">:::(List.append ounit_suite qcheck_suite)


let () =
  run_test_tt_main suite
