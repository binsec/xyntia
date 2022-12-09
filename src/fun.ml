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

(** Functions to sample (used if no json file is given as input) **)
let funs = [|
(fun v -> v.(0) + v.(0) ) ;
(fun v -> v.(0) * v.(0) ) ;
(fun v -> v.(0) - v.(0) ) ;
(fun v -> v.(0) + (v.(0) + v.(0)) ) ;
(fun v -> v.(0) + (v.(0) * v.(0)) ) ;
(fun v -> v.(0) + (v.(0) - v.(0)) ) ;
(fun v -> v.(0) * (v.(0) + v.(0)) ) ;
(fun v -> v.(0) * (v.(0) * v.(0)) ) ;
(fun v -> v.(0) * (v.(0) - v.(0)) ) ;
(fun v -> v.(0) - (v.(0) + v.(0)) ) ;
(fun v -> v.(0) - (v.(0) * v.(0)) ) ;
(fun v -> v.(0) - (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) + (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) * (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) + v.(0)) - (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) + (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) + (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) + (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) * (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) * (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) * (v.(0) - v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) - (v.(0) + v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) - (v.(0) * v.(0)) ) ;
(fun v -> (v.(0) * v.(0)) - (v.(0) - v.(0)) ) ;
|];;
