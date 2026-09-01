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

open Binsec_cli

type out_type = Xyntia | SyGus

module ID = struct
  let name = "xyntiasampler"
end

module Logger = Binsec_sse.Logger.Sub (ID)

module Namespace = Cli.Make_from_logger
    (Logger)
    (struct
      let name = "XyntiaSampler"
      let shortname = "xyntiasampler"
end)

module Mode = Namespace.Builder.Variant_choice_assoc (struct
  type t = out_type

  let name = "type"

  let doc =
    "Type of the samples output (xyntia or sygus; default: xyntia)"

  let default = Xyntia

  let assoc_map = [ ("xyntia", Xyntia); ("sygus", SyGus) ]
end)

module OutDir = Namespace.Builder.String_option (struct
  let name = "out"
  let doc = "set output directory"
end)

module MaxPath = Namespace.Builder.Integer (struct
  let name = "max-paths"
  let doc = "set maximum number of path that can be explored"
  let default = Int.max_int
end)

module Quiet = Namespace.Builder.Boolean (struct
  let name = "quiet"
  let doc = "do not print spinners"
  let default = false
end)