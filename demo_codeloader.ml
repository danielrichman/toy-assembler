open Core.Std

let code =
  let open Assembler.Std in
  assemble
   [ I.ADD { I.source = A.mem ~base:R.RBX (); dest = A.Reg64 R.RAX }
   ; I.DEC (A.Reg64 R.RAX)
   ; I.INC (A.mem ~base:R.RBX ())
   ; I.INC (A.mem ~base:R.RBX ())
   ; I.RET
   ]

let closure : int -> int ref -> int =
  let module C = Codeloader in
  let module S = C.Signature in
  C.load code S.(int @-> ref int @-> int)

let partial = closure 100

let () =
  let r = ref 200 in
  let f () = partial r in
  let one = f () in
  let two = f () in
  let three = f () in
  let four = f () in
  printf "Results: %i %i %i %i\n" one two three four
