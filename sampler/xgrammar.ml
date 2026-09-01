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

open Script

type output = Reg of Ast.Loc.t Ast.loc
type resultout = Dir of string | Stdout | Nothing

type Ast.Obj.t += 
    | Out of output
    | OutList of output list
    | ResOut of resultout


type Ast.t += 
    | SetOutput of resultout
    | Domain of string * int * int
    | OptimalSampling
    | PruneCstOut

type Ast.Instr.t += 
    | Sample of int * output list

let extension = [
          Dyp.Bind_to_cons 
            [
              ("out", "Obj");
              ("list_of_out", "Obj");
              ("res_out", "Obj");
            ];

          Dyp.Add_rules
            [
              (* Ask to sample some or all outputs *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "domain");
                    Dyp.Non_ter ("ident", No_priority);
                    Dyp.Regexp (RE_String "[");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String ",");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String "]");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _;
                      Syntax.String var;
                      _;
                      Syntax.Expr (Ast.Expr.Int (min, _), _); 
                      _;
                      Syntax.Expr (Ast.Expr.Int (max, _), _); 
                      _;
                    ] -> (Syntax.Decl (Domain (var, Z.to_int min, Z.to_int max)), [])
                  | _ -> assert false );
              (* Set the sampling domain for all the inputs *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "domain");
                    Dyp.Regexp (RE_String "all");
                    Dyp.Regexp (RE_String "[");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String ",");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Regexp (RE_String "]");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _; _;
                      Syntax.Expr (Ast.Expr.Int (min, _), _); 
                      _;
                      Syntax.Expr (Ast.Expr.Int (max, _), _); 
                      _;
                    ] -> (Syntax.Decl (Domain ("all", Z.to_int min, Z.to_int max)), [])
                  | _ -> assert false );
              (* Set optimal sampling i.e., with fixed samples and 10% of full random (domains are kept for the remaining) *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "set");
                    Dyp.Regexp (RE_String "optimal");
                    Dyp.Regexp (RE_String "sampling");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _; ] -> (Syntax.Decl OptimalSampling, [])
                  | _ -> assert false );

              (* Ask to prune outputs with no input i.e., which are constant *)
              ( ( "decl",
                  [
                    Dyp.Regexp (RE_String "prune");
                    Dyp.Regexp (RE_String "constant");
                    Dyp.Regexp (RE_String "outputs");
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                    | [ _; _; _;] -> (Syntax.Decl PruneCstOut, [])
                  | _ -> assert false );


              ( ( "fallthrough",
                  [
                    Dyp.Regexp (RE_String "sample");
                    Dyp.Non_ter ("const", No_priority);
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                  | [ _; Syntax.Expr (Ast.Expr.Int (n, _), _); ] -> (Syntax.Instr (Sample (Z.to_int n, [])), [])
                  | _ -> assert false );
              ( ( "fallthrough",
                  [
                    Dyp.Regexp (RE_String "sample");
                    Dyp.Non_ter ("const", No_priority);
                    Dyp.Non_ter ("list_of_out", No_priority); (* list of outputs *)
                  ],
                  "default_priority",
                  [] ),                                                                                                                                                                                            
                fun _ -> function
                  | [ 
                      _; 
                      Syntax.Expr (Ast.Expr.Int (n, _), _);
                      Syntax.Obj (OutList outs); 
                    ] -> (Syntax.Instr (Sample (Z.to_int n, outs)), [])
                  | _ -> assert false );
              ( ( "out",
                  [ Dyp.Non_ter ("var", No_priority); ],
                  "default_priority",
                  [] ),
                fun _ -> function 
                    | [ Syntax.Loc reg; ] -> (Syntax.Obj (Out (Reg reg)), [])
                    | _ -> assert false);
              ( ( "list_of_out",
                  [
                    Dyp.Non_ter ("out", No_priority); 
                    Dyp.Regexp (RE_Char ',');
                    Dyp.Non_ter ("list_of_out", No_priority); 
                  ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [
                      Syntax.Obj (Out out);
                      _;
                      Syntax.Obj (OutList l);
                    ] -> (Syntax.Obj (OutList (out::l)), [])
                  | _ -> assert false );
              ( ( "list_of_out",
                  [ Dyp.Non_ter ("out", No_priority); ],
                  "default_priority",
                  [] ),
                fun _ -> function
                  | [ Syntax.Obj (Out out); ] -> (Syntax.Obj (OutList [out]), [])
                  | _ -> assert false );
            ];
      ]