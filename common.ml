open Core.Std

let is_int8 i  = -128 <= i && i <= 127
let is_int32 i =
     (Int32.to_int_exn Int32.min_value) <= i
  && i <= (Int32.to_int_exn Int32.max_value)

let immediate_size =
  function
  | 0                 -> `Zero
  | i when is_int8  i -> `I8 i
  | i when is_int32 i -> `I32 i
  | i                 -> `I63 i
