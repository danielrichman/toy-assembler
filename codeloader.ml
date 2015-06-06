module Signature = struct
  type 'a t =
    | Arrow : ('a t * 'b t) -> ('a -> 'b) t
    | Int : int t

  let int = Int
  let (@->) a b = Arrow (a, b)

  let arity f =
    let rec loop : type a. int -> a t -> int =
      fun tot g ->
        match g with
        | Int -> tot
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
    let a = Signature.arity signature in
    Printf.printf "load: entry; arity %i\n" a; flush_all ();
    let c = create code a in
    print_endline "load: adding finaliser"; flush_all ();
    Gc.finalise free c;
    print_endline "load: returning"; flush_all ();
    c
