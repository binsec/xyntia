open Libterm.Sexpr

(* Reimplementation of Libterm.Sexpr.eval to be able to read memory not only char by char but with a given size *)

let read_bytes =
  let rec fill_bytes cache value addr size bits i succ =
    if size = 8 then (
      let byte = Bv.to_char value in
      BiTbl.add cache addr byte;
      Bytes.set bits i byte)
    else
      let byte = Bv.to_char (Bv.extract value { hi = 7; lo = 0 }) in
      BiTbl.add cache addr byte;
      Bytes.set bits i byte;
      fill_bytes cache
        (Bv.extract value { hi = size - 1; lo = 8 })
        (Z.add addr (Z.of_int succ))
        (size - 8) bits (i + 1) succ
  in
  let succ (dir : Machine.endianness) =
    match dir with LittleEndian -> 1 | BigEndian -> -1
  in
  let fill_bytes f term addr len (dir : Machine.endianness) cache bits i =
    let addr = Bv.add_int addr (-len) in
    let i = match dir with LittleEndian -> i - len | BigEndian -> i + 1 in
    fill_bytes cache (f term addr len dir) (Bv.value_of addr) (len lsl 3) bits
      i (succ dir)
  in
  let rec read_bytes_hit f term addr len dir cache bits i =
    if len <> 0 then
      match BiTbl.find cache (Bv.value_of addr) with
      | byte ->
          Bytes.set bits i byte;
          read_bytes_hit f term (Bv.succ addr) (len - 1) dir cache bits
            (i + succ dir)
      | exception Not_found ->
          read_bytes_miss f term (Bv.succ addr) (len - 1) dir cache bits
            (i + succ dir)
            1
  and read_bytes_miss f term addr len (dir : Machine.endianness) cache bits i
      len' =
    if len = 0 then fill_bytes f term addr len' dir cache bits i
    else
      match BiTbl.find cache (Bv.value_of addr) with
      | byte ->
          Bytes.set bits i byte;
          fill_bytes f term addr len' dir cache bits i;
          read_bytes_hit f term (Bv.succ addr) (len - 1) dir cache bits
            (i + succ dir)
      | exception Not_found ->
          read_bytes_miss f term (Bv.succ addr) (len - 1) dir cache bits
            (i + succ dir)
            (len' + 1)
  in
  fun f term addr len (dir : Machine.endianness) cache ->
    let bits = Bytes.create len in
    read_bytes_hit f term addr len dir cache bits
      (match dir with LittleEndian -> 0 | BigEndian -> len - 1);
    Bv.create (Z.of_bits (Bytes.unsafe_to_string bits)) (len lsl 3)

let rec eval
    ?(symbols =
      fun e -> Bitvector.create (Z.of_int (Expr.hash e)) (Expr.sizeof e))
    ?(memory = fun _ _ size _ -> Bv.zeros (size lsl 3))
    ((vars, values, _, _, _) as m) = function
  | Expr.Cst bv -> bv
  | e -> (
      try BvTbl.find values e
      with Not_found ->
        let value =
          match e with
          | Expr.Cst _ -> assert false
          | Expr.Var { name; _ } ->
              StTbl.add vars name e;
              symbols e
          | Expr.Load { addr; len; dir; label; _ } ->
              eval_load ~symbols ~memory m
                (eval ~symbols ~memory m addr)
                len dir label
          | Expr.Unary { f; x; _ } ->
              Term.Bv.unary f (eval ~symbols ~memory m x)
          | Expr.Binary { f; x; y; _ } ->
              Term.Bv.binary f
                (eval ~symbols ~memory m x)
                (eval ~symbols ~memory m y)
          | Expr.Ite { c; t; e; _ } ->
              if Bv.zero = eval ~symbols ~memory m c then
                eval ~symbols ~memory m e
              else eval ~symbols ~memory m t
        in
        BvTbl.add values e value;
        value)
and eval_load ~symbols ~memory ((_, _, cache, arrays, _) as t) ptr len dir
        (memory_term : Memory.t) =
      match memory_term with
      | Root -> read_bytes memory memory_term ptr len dir cache
      | Symbol n ->
          read_bytes memory memory_term ptr len dir
            (try StTbl.find arrays n
             with Not_found ->
               let arr = BiTbl.create 16 in
               StTbl.add arrays n arr;
               arr)
      | Layer { addr; store; over; _ } ->
          let addr = eval ~symbols ~memory t addr in
          let size = Bv.size_of addr in
          let offset = Bv.sub ptr addr in
          let miss i s =
            Chunk.of_term
              (Expr.load s Expr.LittleEndian
                 (Expr.constant (Bv.add addr (Bv.create i size)))
                 over)
          in
          let bytes = Chunk.to_term (Store.select miss offset len store) in
          let bytes =
            match dir with LittleEndian -> bytes | BigEndian -> bswap bytes
          in
          eval ~symbols ~memory t bytes
  
