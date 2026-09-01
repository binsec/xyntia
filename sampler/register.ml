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

open Binsec_sse.Types
module Sexpr = Binsec_symbolic.Default


module MakeExtension (Engine : ENGINE with type Path.value = Sexpr.Expr.t) (ContextGetter : Termsampler.CONTEXTGETTER with type state = Engine.Path.state) 
    : EXTENSIONS with type path = Engine.Path.t = struct

  include Termsampler.XyntiaSamplerUtils (Engine) (ContextGetter)

  let list = [ 
    Grammar_extension Xgrammar.extension ;
    Instruction_resolver instruction_callback;
    Command_handler declaration_callback;
    Builtin_resolver builtin_callback;
    Exit_callback at_exit_callback;
    Signal_callback signal_callback;
    Initialization_callback init_callback;
  ]
end

module XyntiaSamplerPlugin : PLUGIN = struct
  include Xcli.ID

  let fields : (module PATH
         with type value = 'value
          and type model = 'model
          and type state = 'state
          and type t = 'path) ->
      ('value, 'model, 'state, 'path) field list =
   fun _ -> []

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Engine = (val engine) in
    let module MS = Engine.Path.State in
    match MS.more Symbolic.State.ValueKind, MS.more Symbolic.Default.State.VisibleMemory, 
          MS.more Symbolic.Default.State.VisibleSymbols,
          MS.more Symbolic.State.ToFormula with
    | Some Symbolic.Default.Term, Some get_vmem, Some get_vsymb, Some toformula -> 
      let module Extensions = MakeExtension (Engine) (struct
        type state = Engine.Path.state
        let symb = get_vsymb
        let mem = get_vmem
        let toformula = toformula
      end) in
      Extensions.list
    | _ -> assert false
end


let () = 
    (* Register the plugin with DBA instruction to set permissions *)
    Binsec_cli_sse.Plugins.register ~is_enabled:Xcli.Namespace.is_enabled (fun () ->
      (module XyntiaSamplerPlugin));
