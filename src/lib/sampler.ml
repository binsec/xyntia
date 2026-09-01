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

module Libsse = Binsec_cli_sse

type samplekind = File of string | Json of Yojson.Basic.t
type groundtruth = SmtFile of string | BinsecFormula of Binsec_smtlib.Formula.formula | Nothing

let outdir = ref None

let sample ?sse_depth:sd ?sse_maxpaths:sm ?quiet:(quiet=false) binary config isa_opt out_opt = 
  Libsse.Options.Engine.set Libsse.Options.Engine.Default;
  Libsse.Options.ScriptFiles.set [config];
  Binsec.Kernel_options.ExecFile.set binary;
  Libsse.Options.Logger.quiet ();

  (match isa_opt with 
  | Some "x86" -> Binsec.Kernel_options.Machine.set Binsec.Machine.x86
  | Some "amd64" -> Binsec.Kernel_options.Machine.set Binsec.Machine.amd64
  | Some "arm32" -> Binsec.Kernel_options.Machine.set Binsec.(Machine.armv7 LittleEndian) 
  | Some "aarch64" -> Binsec.Kernel_options.Machine.set Binsec.(Machine.armv8 LittleEndian) 
  | Some "ppc64" -> Binsec.Kernel_options.Machine.set Binsec.(Machine.ppc64 BigEndian) 
  | Some "riscv" -> Binsec.Kernel_options.Machine.set Binsec.(Machine.riscv `x32) 
  | Some "riscv64" -> Binsec.Kernel_options.Machine.set Binsec.(Machine.riscv `x64) 
  | None -> (
    Binsec.Kernel_functions.Loader.set_arch_from_file ~filename:binary;
    (match Binsec.Kernel_options.Machine.get () with
    | Unknown -> Binsec.Kernel_options.Machine.set Binsec.Machine.x86
    | _ -> ()))
  | _ -> failwith "unknown ISA");
  
  Xyntiasampler.Xcli.Namespace.enable ();
  Xyntiasampler.Xcli.Mode.set (Xyntiasampler.Xcli.Xyntia);

  (match out_opt with 
  | Some d -> Xyntiasampler.Xcli.OutDir.set d
  | None -> ());

  Xyntiasampler.Xcli.Quiet.set quiet;

  (match sd with
  | Some d -> 
    Libsse.Options.MaxDepth.set d;
  | None -> ());

  (match sm with
  | Some d -> Xyntiasampler.Xcli.MaxPath.set d;
  | None -> ());
  
  Libsse.Options.Search_heuristics.set Dfs;
  Libsse.Options.enable ();
  Libsse.Sse.run ()


let get_formula () =
  BinsecFormula (!Xyntiasampler.Termsampler.out_formula |> Option.get)

let get_samples () =
    List.map (fun a -> Json a) !Xyntiasampler.Termsampler.json_samples

let remove_samples () =
  FileUtil.rm ~recurse:true [Option.get !outdir];
  outdir := None
