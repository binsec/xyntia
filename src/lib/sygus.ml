open Format 
 (* let template = "\
  (set-logic BV)

  (synth-fun f ({args}) (_ BitVec {outsize})
      (
          (Start (_ BitVec {outsize})) 
          {"(EightBytes (_ BitVec 64))" if outsize == 64 else ("(FourBytes (_ BitVec 32))" if outsize == 32 else ("(TwoBytes (_ BitVec 16))" if outsize == 16 else ("(Eightbits (_ BitVec 8))" if outsize == 8 else None)))}
          {"(EightBytes (_ BitVec 64))" if len(vars64)  > 0 and outsize != 64 else ""}
          {"(FourBytes (_ BitVec 32))"  if len(vars32) > 0 and outsize != 32 else ""}
          {"(TwoBytes (_ BitVec 16))"   if len(vars16) > 0 and outsize != 16 else ""}
          {"(Eightbits (_ BitVec 8))"   if len(vars8) > 0 and outsize != 8 else ""}
      ) (
      (Start (_ BitVec {outsize}) (
          {"EightBytes" if outsize == 64 else ("FourBytes" if outsize == 32 else ("TwoBytes" if outsize == 16 else ("Eightbits" if outsize == 8 else None)))}
      ))
      {get_glue(vars8, vars16, vars32, vars64, outsize)}
      {gram64 if len(vars64) > 0 else ""}
      {gram32 if len(vars32) > 0 else ""}
      {gram16 if len(vars16) > 0 else ""}
      {gram8  if len(vars8)  > 0 else ""}
  ))
  
  {constrs}
  
  (check-synth)" *)


(* 
(EightBytes (_ BitVec 64) (#x0000000000000001 {vars64}
*)

let nonterm_of_size = function
| 8 -> "OneByte"
| 16 -> "TwoBytes"
| 32 -> "FourBytes"
| 64 -> "EightBytes"
| _ -> assert false

module SyGusPrinter (O: Oracle.ORACLE) (MU : Tree.MUTATIONS) = struct

  let get_sz_modifiers sz all_sizes = 
    let res = List.fold_left (fun acc ext_sz -> 
      if sz == ext_sz then acc
      else if sz > ext_sz then 
        let zext = Format.sprintf "((_ zero_extend %d) %s)" (sz - ext_sz) (nonterm_of_size ext_sz) in
        let sext = Format.sprintf "((_ sign_extend %d) %s)" (sz - ext_sz) (nonterm_of_size ext_sz) in
        acc ^ zext ^ " " ^ sext ^ " "
      else 
        let red = Format.sprintf "((_ extract %d 0) %s)" (sz-1) (nonterm_of_size ext_sz) in
        acc ^ red ^ " "
    ) "" all_sizes in
    res

  let get_rule sz all_sizes =
    let res = sprintf "(%s (_ BitVec %d) (" (nonterm_of_size sz) sz in
    let res = Array.fold_left (fun acc (op : Tree.op_t) -> 
      acc ^ (MU.unop_sygus op (nonterm_of_size sz) sz) ^ " ") res (MU.unop_mut ()) in
    let res = Array.fold_left (fun acc (op : Tree.op_t) -> 
      acc ^ (MU.binop_sygus op (nonterm_of_size sz) sz (nonterm_of_size sz) sz) ^ " ") res (MU.binop_mut ()) in
    let res = Array.fold_left (fun acc (op : Tree.op_t) -> 
      acc ^ (MU.triop_sygus op (nonterm_of_size sz) sz (nonterm_of_size sz) sz (nonterm_of_size sz) sz) ^ " ") res (MU.triop_mut ()) in

    (* Extenstions *)
    let res = res ^ get_sz_modifiers sz all_sizes in
    res ^ "))\n"


  let print () = 
    let nvars = O.nvars () in
    let ovar = O.out_var () in
    let osize = ovar.sz in
    let invars = O.vars () in
    assert (Array.length invars == nvars);
    
    (* Create (synth-fun ...)*)
    let synth_fun = (Array.fold_left (fun acc (var : Oracle.variable) -> 
      let n = sprintf " (%s (_ BitVec %d)) " var.name var.sz in
      acc ^ n
    ) "(synth-fun f (" invars) ^ sprintf ") (_ BitVec %d)\n" osize in

    (* Declare non terminal symbols *)
    let decl = 
      (sprintf "\t((Start (_ BitVec %d))\n" osize) 
      (* ^
      (sprintf "\t(%s (_ BitVec %d))\n" (nonterm_of_size osize) osize)  *)
    in
    let input_sizes = Array.fold_left (fun acc (var : Oracle.variable) -> 
      let sz = var.sz in  
      if List.mem sz acc then acc
      else sz::acc
    ) [] invars in
    let decl = List.fold_left (fun acc sz -> 
      (* if sz == osize then acc
      else  *)
      acc ^ sprintf "\t(%s (_ BitVec %d))\n" (nonterm_of_size sz) sz
    ) decl input_sizes in
    let decl = 
      if List.mem osize input_sizes then decl
      else decl ^ sprintf "\t(%s (_ BitVec %d))\n" (nonterm_of_size osize) osize
    in
    let decl = decl ^ ") (\n" in

    (* Define rules *)
    let rules = sprintf "\t(Start (_ BitVec %d) (%s))\n" osize (nonterm_of_size osize) in
    let rules = List.fold_left (fun acc sz -> 
      acc ^ "\t" ^ (get_rule sz input_sizes)) rules input_sizes in

    (* add extensions *)
    let glue = if List.mem osize input_sizes then ""
    else
      sprintf "\t(%s (_ BitVec %d) (%s))\n" (nonterm_of_size osize) osize (get_sz_modifiers osize input_sizes)
    in

    let rules = rules ^ glue ^ "))\n" in
    
    (* Add samples *)
    let samples = Array.init (O.nsamples ()) (fun i -> 
      let s = O.get_sample i in
      let inputs = O.sample_inputs s in
      let _ovar, obv = O.sample_output s in
      let constr = "(constraint (= (f" in
      let constr = Array.fold_left (fun acc (_, ibv) -> 
        let v = (Utility.bv_to_smtlib ibv) in
        acc ^ " " ^ v) constr inputs in
      constr ^ sprintf ") %s))" (Utility.bv_to_smtlib obv) 
    )
    in
    let samples = (Array.fold_left (fun acc s -> acc ^ "\n" ^ s) "" samples) ^ "\n" in

    let checksynth = "(check-synth)" in
    "(set-logic BV)\n" ^ synth_fun ^ decl ^ rules ^ samples ^ checksynth
end
  
  