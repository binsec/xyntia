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

open Progress
module Ansi = Terminal.Style

let apply_color color s = Ansi.(code color) ^ s ^ Ansi.(code none)

let pick_colour () = Color.ansi `blue

(** Examples taken from: https://github.com/sindresorhus/cli-spinners/ *)

include struct
  let spin frames min_interval = Line.spinner ~color:(pick_colour ()) ~frames ~min_interval ()
  let dots1 = spin [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ]
end

let spinner1 = Line.(lpad 25 (dots1 (Some (Duration.of_int_ms 80)) 
                      ++ (Line.of_printer (Printer.int ~width:10)) 
                      ++ (const " path explored ") 
                      ++ (brackets (elapsed ()))))
                    
let spinner2 = Line.(lpad 25 (dots1 (Some (Duration.of_int_ms 80)) 
                      ++ (Line.of_printer (Printer.int ~width:10)) 
                      ++ (const " samples generated ") 
                      ++ (brackets (elapsed ()))))

let spinner3 = Line.(lpad 25 (dots1 (Some (Duration.of_int_ms 80))
                      ++ (const "      Saving samples ") 
                      ++ (brackets (elapsed ()))))

let is_finished finished mutex =
  Mutex.lock mutex;
  let res = !finished in
  Mutex.unlock mutex;
  res

let report1 reporter mutex npaths =
  let pathnum = (
      Mutex.lock mutex;
      let r = !npaths in
      Mutex.unlock mutex;
      r
    ) in
    Reporter.report reporter pathnum

let report2 reporter mutex n =
  let pathnum = (
      Mutex.lock mutex;
      let r = !n in
      Mutex.unlock mutex;
      r
    ) in
    Reporter.report reporter pathnum

let run (finished, npaths, nsamples, asked_samples, samples_saved, end_process, mutex) =
  let display = Display.start Multi.(blank) in
  let reporter1 = Display.add_line display spinner1 in
  (while (not (is_finished finished mutex)) && (not (is_finished end_process mutex))  do
    report1 reporter1 mutex npaths;
    Thread.yield ()
  done);
  Thread.yield ();
  report1 reporter1 mutex npaths;
  Reporter.finalise reporter1;
  let reporter1_bis = Display.add_line ~above:1 display (Line.const "✔️") in
  Display.remove_line display reporter1;


  let reporter2 = Display.add_line display spinner2 in
  (while (not (!nsamples == !asked_samples)) && (not (is_finished end_process mutex)) do
    report2 reporter2 mutex nsamples;
    Thread.yield ()
  done);
  Thread.yield ();
  report2 reporter2 mutex nsamples;
  Reporter.finalise reporter2;
  let reporter2_bis = Display.add_line ~above:1 display (Line.const "✔️") in
  Display.remove_line display reporter2;


  let reporter3 = Display.add_line display spinner3 in
  (while (not (is_finished samples_saved mutex)) && (not (is_finished end_process mutex)) do
    Reporter.report reporter3 1;
    Thread.yield ()
  done);
  Thread.yield ();
  report2 reporter3 mutex nsamples;
  Reporter.finalise reporter3;
  let reporter3_bis = Display.add_line ~above:1 display (Line.const "✔️") in
  Display.remove_line display reporter3;


  Display.remove_line display reporter1_bis;
  Display.remove_line display reporter2_bis;
  Display.remove_line display reporter3_bis;

  Display.finalise display

let thread = ref None

let mutex = Mutex.create ()

let create (exploration_finished, npaths, nsuccess_sampling, asked_nsamples, samples_saved, end_process) = 
  if Xcli.Quiet.get () then () else ( 
  thread := Some (Thread.create run (exploration_finished, npaths, nsuccess_sampling, asked_nsamples, samples_saved, end_process, mutex)))

let stop () = 
  if Xcli.Quiet.get () then () else (
  Thread.yield ();
  Thread.join (Option.get !thread))

let update f = 
  let r = if Xcli.Quiet.get () then 
      f ()
    else (
      Mutex.lock mutex;
      let r' = f () in
      Mutex.unlock mutex;
      r'
    )
    in
  r