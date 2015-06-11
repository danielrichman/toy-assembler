open Core.Std

module Register : sig
  (* Ordering & info from /asmcomp/amd64/proc.ml *)
  type t =
    | RAX
    | RBX
    | RDI
    | RSI
    | RDX
    | RCX
    | R8
    | R9
    | R12
    | R13
    | R10
    | R11
    | RBP
    | R14
    | R15
    | RSP
    | RIP

  val ocaml_use : t -> [ `General of int | `Trap_pointer | `Allocation_pointer
                       | `Stack_pointer | `Instruction_pointer ]
 
  val calling_args : t list
  val calling_ret  : t

  val trap_pointer : t
  val allocation_pointer : t
  val stack_pointer : t
  val instruction_pointer : t

  val to_string_gas : t -> string
end

module Opcode : sig
  type t
  val to_int : t -> int
  val to_char : t -> char
  val to_string_hum : t -> string
end

module Operand : sig
  type index =
    { value : Register.t
    ; scale : int
    }

  type mem =
    { base : Register.t option
    ; index : index option
    ; offset : int
    }

  type t =
    | Imm   of int
    | Reg64 of Register.t
    | Mem64 of mem

  val mem : ?base:Register.t -> ?index:(Register.t * int) -> ?offset:int -> unit -> t

  val to_string_gas : t -> string
end

module Instruction : sig
  type add = { source : Operand.t; dest : Operand.t }

  type t =
    | ADD of add
    | INC of Operand.t
    | DEC of Operand.t
    | RET

  type encoded = [ `Op of Opcode.t | `LE32 of int | `I8 of int ] list

  val parts : t -> encoded

  val assemble_into : t -> (read_write, Iobuf.seek) Iobuf.t -> unit

  val to_string_gas : t -> string
  val to_string_assembled : t -> string
  val t_list_to_string_assembled : t list -> string
end

module Std : sig
  module R = Register
  module A = Operand
  module I = Instruction
  val assemble : I.t list -> string
end
