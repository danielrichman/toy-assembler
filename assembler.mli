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
end

module Opcode : sig
  type t
  val to_int : t -> int
  val to_char : t -> char
end

module Operand : sig
  type index =
    { value : Register.t
    ; scale : int
    }

  type regmem =
    { base : Register.t
    ; index : index option
    ; offset : int
    }

  type t =
    | Imm   of int
    | Reg64 of Register.t
    | RegMem64 of regmem
    | Mem64 of int

  val regmem : ?index:(Register.t * int) -> ?offset:int -> Register.t -> regmem
end

module Instruction : sig
  type add = { source : Operand.t; dest : Operand.t }

  type t =
    | ADD of add

  type encoded = [ `Op of Opcode.t | `LE32 of int | `I8 of int ] list

  val parts : t -> encoded

  val encode_into : t -> (read_write, _) Iobuf.t -> unit
end
