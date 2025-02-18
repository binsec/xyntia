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

let outdir = ref None

let sample binary config isa_opt out_opt = 
  Libsse.Options.Engine.set Libterm.Senv.Vanilla;
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
  | None -> Binsec.Kernel_options.Machine.set Binsec.Machine.x86;
      Binsec.Kernel_functions.Loader.set_arch_from_file ~filename:binary;
  | _ -> failwith "unknown ISA");
  
  Xyntiasampler.Termsampler.enable ();
  Xyntiasampler.Termsampler.Mode.set (Xyntiasampler.Termsampler.Xyntia);

  outdir := (match out_opt with 
  | Some _ -> out_opt
  | None -> Some (Printf.sprintf "/tmp/xyntiasampling_%d" (Unix.getpid ())));

  Xyntiasampler.Termsampler.OutDir.set (Option.get !outdir);
  let module Eng = (val (Libsse.Options.Engine.get_factory ())) in
  let module R = Libsse.Exec.Run (Eng) (Libsse.Heuristic.Dfs) () in
  R.unit

let get_formula () =
  (Option.get !outdir) ^ "/formula"

let get_samples () =
    Sys.readdir (Option.get !outdir)
    |> Array.to_list
    |> List.filter (fun x -> Filename.extension x = ".json")
    |> List.map (fun x -> (Option.get !outdir) ^ "/" ^ x) 

let remove_samples () =
  FileUtil.rm ~recurse:true [Option.get !outdir];
  outdir := None