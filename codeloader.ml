open Core.Std

module Signature = struct
  type 'a t =
    | Arrow : ('a t * 'b t) -> ('a -> 'b) t
    | Int : int t
    | Ref : 'a t -> 'a ref t
    | Iobuf : (read_write, _) Iobuf.t t

  let int = Int
  let ref x = Ref x
  let iobuf = Iobuf
  let (@->) a b = Arrow (a, b)

  let arity f =
    let rec loop : type a. int -> a t -> int =
      fun tot g ->
        match g with
        | Int -> tot
        | Ref _ -> tot
        | Iobuf -> tot
        | Arrow (_, b) -> loop (tot + 1) b
    in
    loop 0 f
end

(* There are some subtleties with regards to C calls and currying.
 * If I wrote "bytes -> int -> ('a -> 'b)" it would expect the
 * codeloader_create function to take three arguments and return a 'b.
 * This is not what we want. This, however, does (hopefully) do what we want: *)
type ('a, 'b) closure = 'a -> 'b
external create : bytes -> int -> ('a, 'b) closure = "codeloader_create"
external free   : ('a, 'b) closure -> unit = "codeloader_free"

let () =
  Callback.register "codeloader_fail_freed"
    (fun () -> failwith "closure freed");
  Callback.register "codeloader_fail_curry"
    (fun () -> failwith "no caml_curry for this arity")

let load : string -> ('a -> 'b) Signature.t -> ('a -> 'b) =
  fun code signature ->
    let c = create code (Signature.arity signature) in
    Caml.Gc.finalise free c;
    c
