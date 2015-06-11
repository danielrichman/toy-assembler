module Signature : sig
  type 'a t
  
  val int : int t
  val ref : 'a t -> 'a ref t
  val (@->) : 'a t -> 'b t -> ('a -> 'b) t
end

val load : string -> ('a -> 'b) Signature.t -> ('a -> 'b)
