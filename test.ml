(*    0x0000000000000000 <+0>: 48 ff c0  inc    %rax
 *    0x0000000000000003 <+3>: c3        retq        *)

let _code = "\x48\xff\xc0\xc3"

(*    0x0000000000000000 <+0>: 48 01 d8  add    %rbx,%rax
 *    0x0000000000000003 <+3>: 48 ff c8  dec    %rax
 *    0x0000000000000006 <+6>: c3        retq              *)

let code = "\x48\x01\xd8\x48\xff\xc8\xc3"

let closure : int -> int -> int =
  let module C = Codeloader in
  let module S = C.Signature in
  C.load code S.(int @-> int @-> int)

let partial = closure 100

let () =
  let r = partial 200 in
  Printf.printf "Result: %i\n" r
