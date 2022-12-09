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

open Printf

let kernel_section = [
    ("isa", "x86") ;
]

let mk_header = sprintf "[%s]"

let mk_section hdr_name =
    List.fold_left
    (fun acc (key, vl) -> sprintf "%s\n%s = %s" acc key vl)
    (mk_header hdr_name)

let mk_xyntia_sec binfile (output : Oracle.variable) expr =
    let return_val =
        sprintf "%s<%d>" output.name output.sz
    in
    mk_section "xyntia"
    [
        ("bin", binfile)       ;
        ("return", return_val) ;
        ("challenger", expr)   ;
    ]

let mk_confs ~binfile ~output ~expr =
    let xyntia_section =
        mk_xyntia_sec binfile output expr
    in
    String.concat "\n\n" [
        mk_section "kernel"
            kernel_section; xyntia_section
    ]
