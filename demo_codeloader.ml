open Core.Std

let code =
  let open Assembler.Std in
  assemble
   [ I.ADD { I.source = A.Reg64 R.RBX; dest = A.Reg64 R.RAX }
   ; I.DEC (A.Reg64 R.RAX)
   ; I.RET
   ]

let closure : int -> int -> int =
  let module C = Codeloader in
  let module S = C.Signature in
  C.load code S.(int @-> int @-> int)

let partial = closure 100

let () =
  let r = partial 200 in
  printf "Result: %i\n" r
