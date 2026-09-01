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

type thread_data = {
  thread : Thread.t;
  mutex : Mutex.t;
  finished : bool ref;
} 

let tdata = ref None

let quiet = ref false

let run_spinner (s, finished, mutex) =
    let open Progress in 

    let pick_colour () = Progress.Color.ansi `blue in

    let spin frames min_interval = Progress.Line.spinner ~color:(pick_colour ()) ~frames ~min_interval () in
    let dots1 = spin [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] in


    let spinner1 = Progress.Line.(lpad 25 (dots1 (Some (Progress.Duration.of_int_ms 80)) 
                      ++ (const s))) in
    
    let is_finished finished mutex =
        Mutex.lock mutex;
        let res = !finished in
        Mutex.unlock mutex;
        res
    in

    let display = Display.start Multi.(blank) in
    let reporter1 = Display.add_line display spinner1 in
    (while not (is_finished finished mutex) do
        Reporter.report reporter1 ();
        Thread.yield ()
    done);
    Thread.yield ();
    Reporter.report reporter1 ();
    Reporter.finalise reporter1;
    Display.remove_line display reporter1;
    Display.finalise display

let create msg = 
  if !quiet then () else
  let mutex = Mutex.create () in
  let finished = ref false in
  let thread = Thread.create run_spinner (msg, finished, mutex) in
  tdata := Some { thread; mutex; finished }

let stop () = 
  if !quiet then () else
  let d = Option.get !tdata in
  (Mutex.lock d.mutex; d.finished := true; Mutex.unlock d.mutex);
  Thread.join d.thread