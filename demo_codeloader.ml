open Core.Std

let code =
  let open Assembler.Std in
  let arg1, arg2, arg3 =
    match R.calling_args with
    | a::b::c::_ -> (a, b, c)
    | _ -> assert false
  in
  assemble
   [ (* a1 := Iobuf.underlying_bigstring a1 *)
     I.MOV { I.source = A.mem ~base:arg1 (); dest = A.Reg64 arg1 }
     (* a1 := Bigstring.underlying_string_ptr a1 *)
   ; I.MOV { I.source = A.mem ~base:arg1 ~offset:8 (); dest = A.Reg64 arg1 }

     (* un-tag ints *)
   ; I.SHR (A.Reg64 arg2, 1)
   ; I.SHR (A.Reg64 arg3, 1)
     (* a1[a2] := a3 *)
   ; I.MOV { I.source = A.Reg64 arg3
           ; dest = A.mem ~base:arg1 ~index:(arg2, 8) ()
           }

     (* return a2 + 1 *)
   ; I.ADD { I.source = A.Imm 1; dest = A.Reg64 arg2 }
   ; I.SHL (A.Reg64 arg2, 1)
   ; I.BTS { I.bit_no = A.Imm 0; test_val = A.Reg64 arg2 }
   ; I.MOV { I.source = A.Reg64 arg2; dest = A.Reg64 R.calling_ret }
   ; I.RET
   ]

let closure : (read_write, _) Iobuf.t -> int -> int -> int =
  let module C = Codeloader in
  let module S = C.Signature in
  C.load code S.(iobuf @-> int @-> int @-> int)

let scratch = Iobuf.create ~len:64
let partial = closure scratch

let () =
  let one   = partial 0 0x6161616161616165 in
  let two   = partial 1 0x6262626262626266 in
  let three = partial 2 0x6363636363636367 in
  let four  = partial 3 0x6464646464646468 in
  printf "Results: %i %i %i %i\n" one two three four;
  print_endline (Iobuf.to_string_hum scratch)
