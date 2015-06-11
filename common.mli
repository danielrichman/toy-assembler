open Core.Std

val is_int8 : int -> bool
val is_int32 : int -> bool
val immediate_size : int -> [ `Zero | `I8 of int | `I32 of int | `I63 of int ]
