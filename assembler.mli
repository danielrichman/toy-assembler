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

  (* In general, I'm only implementing operations on 64bit registers.
   * See Instruction.SET *)
  module B8 : sig
    type reg64 = t
    type t =
      | AL
      | BL
      | CL
      | DL

    val of_reg64 : reg64 -> t
    val to_string_gas : t -> string
  end
end

module Opcode : sig
  type t
  val to_ints : t -> int list
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
  type binary_op = { source : Operand.t; dest : Operand.t }

  type set_condition =
    [ `OF1 | `OF0
    | `CF1 | `CF0
    | `ZF1 | `ZF0
    | `CF1_or_ZF1 | `CF0_and_ZF0
    | `SF1 | `SF0
    | `PF1 | `PF0 
    | `SF_ne_OF | `SF_eq_OF
    | `ZF1_or_SF_ne_OF | `ZF0_and_SF_eq_OF
    ]

  type t =
    | ADD of binary_op
    | INC of Operand.t
    | DEC of Operand.t
    | SHL of Operand.t * int
    | SHR of Operand.t * int
    | MOV of binary_op
    | RET
      (* In general, I'm only implementing operations on 64bit registers.
       * However, the SETcc family only operates on bits 0..7 or 8..15 of
       * registers A, B, C, D. MOVZBL only operates on bits 0..7.
       * These two instructions, a special case, are required to get things
       * out of processor flags.
       * I have only implemented a subset of SET & MOVZX. *)
    | SET of set_condition * Register.B8.t
    | MOVZBQ of Register.B8.t * Register.t

  type encoded = [ `Op of Opcode.t | `LE32 of int | `LE64 of int | `I8 of int ] list

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
