open Core.Std
open Common

module Register = struct
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

  let ocaml_use = function
    | RAX -> `General 0
    | RBX -> `General 1
    | RDI -> `General 2
    | RSI -> `General 3
    | RDX -> `General 4
    | RCX -> `General 5
    | R8  -> `General 6
    | R9  -> `General 7
    | R12 -> `General 8
    | R13 -> `General 9
    | R10 -> `General 10
    | R11 -> `General 11
    | RBP -> `General 12
    | R14 -> `Trap_pointer
    | R15 -> `Allocation_pointer
    | RSP -> `Stack_pointer
    | RIP -> `Instruction_pointer

  (* XXX: untrue for float args/return values *)
  let calling_args = [RAX; RBX; RDI; RSI; RDX; RCX; R8; R9; R12; R13]
  let calling_ret = RAX
  let trap_pointer = R14
  let allocation_pointer = R15
  let stack_pointer = RSP
  let instruction_pointer = RIP

  let operand_number = function
    | RAX -> 0
    | RCX -> 1
    | RDX -> 2
    | RBX -> 3
    | RSP -> 4
    | RBP -> 5
    | RSI -> 6
    | RDI -> 7
    | R8  -> 8 + 0
    | R9  -> 8 + 1
    | R10 -> 8 + 2
    | R11 -> 8 + 3
    | R12 -> 8 + 4
    | R13 -> 8 + 5
    | R14 -> 8 + 6
    | R15 -> 8 + 7
    | RIP -> failwith "RIP can't be used as an operand"

  let to_string_gas = function
    | RAX -> "%rax"
    | RBX -> "%rbx"
    | RDI -> "%rdi"
    | RSI -> "%rsi"
    | RDX -> "%rdx"
    | RCX -> "%rcx"
    | R8  -> "%r8"
    | R9  -> "%r9"
    | R12 -> "%r12"
    | R13 -> "%r13"
    | R10 -> "%r10"
    | R11 -> "%r11"
    | RBP -> "%rbp"
    | R14 -> "%r14"
    | R15 -> "%r15"
    | RSP -> "%rsp"
    | RIP -> "%rip"

  module B8 = struct
    type reg64 = t
    type t =
      | AL
      | BL
      | CL
      | DL

    let of_reg64 =
      function
      | RAX -> AL
      | RBX -> BL
      | RCX -> CL
      | RDX -> DL
      | _ -> failwith "Register does not have a B8 part"

    let operand_number = function
      | AL -> 0
      | BL -> 3
      | CL -> 1
      | DL -> 2

    let to_string_gas = function
      | AL -> "%al"
      | BL -> "%bl"
      | CL -> "%cl"
      | DL -> "%dl"
  end
end

module Operand = struct
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
    | Imm   of int          (* XXX: really should be int64 *)
    | Reg64 of Register.t
    | Mem64 of mem

  let mem ?base ?index ?(offset=0) () =
    let index = Option.map index ~f:(fun (a, b) -> { value = a; scale = b }) in
    Mem64 { base; index; offset }

  let to_string_gas = function
    | Imm i -> sprintf "$%i" i
    | Reg64 r -> Register.to_string_gas r
    | Mem64 { base = None; index = None; offset } -> sprintf "0x%x" offset
    | Mem64 { base; index; offset } ->
      let base =
        match base with
        | Some r -> Register.to_string_gas r
        | None -> ""
      in
      let index =
        match index with
        | Some { value; scale } -> sprintf ",%s,%i" (Register.to_string_gas value) scale
        | None -> ""
      in
      let offset =
        match offset with 
        | 0 -> ""
        | i -> Int.to_string i
      in
      sprintf "%s(%s%s)" offset base index
end

(* XXX I'm not sure this should really be called 'Opcode' *)
module Opcode = struct
  module RexFlags : sig
    type t = private int
    val empty : t
    val set : ?w:bool -> ?r:bool -> ?x:bool -> ?b:bool -> t -> t
    val to_int : t -> int
    val is_empty : t -> bool
    val to_string : t -> string
  end = struct
    type t = int

    let empty = 0
    let w = 8
    let r = 4
    let x = 2
    let b = 1

    let set ?w:sw ?r:sr ?x:sx ?b:sb t =
      let set_or_clear t action value =
        match action with
        | None -> t
        | Some false -> t land (lnot value)
        | Some true  -> t lor  value
      in
      let t = set_or_clear t sw w in
      let t = set_or_clear t sr r in
      let t = set_or_clear t sx x in
      let t = set_or_clear t sb b in
      t

    let is_empty t = (t = 0)
    let to_int t = t
    let to_string t =
      let letter v c =
        if t land v = 0 then "" else Char.to_string c
      in
      letter w 'w' ^ letter r 'r' ^ letter x 'x' ^ letter b 'b'
  end

  type modrm = { mod_ : int; reg : int; rm : int }
  type sib = { base : int; index : int; scale : int }

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
    | REX of RexFlags.t
    | ModRM of modrm
    | SIB of sib
    | ADD of [ `imm32_RAX | `imm32_rm64 | `imm8_rm64 | `r64_rm64 | `rm64_r64 ]
    | AND of [ `imm32_RAX | `imm32_rm64 | `imm8_rm64 | `r64_rm64 | `rm64_r64 ]
    | OR  of [ `imm32_RAX | `imm32_rm64 | `imm8_rm64 | `r64_rm64 | `rm64_r64 ]
    | XOR of [ `imm32_RAX | `imm32_rm64 | `imm8_rm64 | `r64_rm64 | `rm64_r64 ]
    | INC
    | DEC
    | SHL of [ `one | `imm8 ]
    | SHR of [ `one | `imm8 ]
    | MOV of [ `r64_rm64 | `rm64_r64 | `moffset64_RAX | `RAX_moffset64
             | `imm64_r64 of int | `imm32_rm64 ]
    | RET
    | SET of set_condition
    | MOVZBQ
    | PUSH of [ `rm64 | `r64 of int | `imm8 | `imm32 ]

  let to_ints = function
    | REX fls -> [ 0x40 lor (RexFlags.to_int fls) ]
    | ModRM { mod_; reg; rm } ->
      assert (0 <= mod_ && mod_ <= 3);
      assert (0 <= reg  && reg  <= 7);
      assert (0 <= rm   && rm   <= 7);
      [ (mod_ lsl 6) lor (reg  lsl 3) lor rm ]
    | SIB { base; index; scale } ->
      assert (0 <= base  && base <= 7);
      assert (0 <= index && index <= 7);
      assert (0 <= scale && scale <= 3);
      [ (scale lsl 6) lor (index lsl 3) lor base ]
    | ADD `imm32_RAX  -> [ 0x05 ]
    | ADD `imm32_rm64 -> [ 0x81 ]
    | ADD `imm8_rm64  -> [ 0x83 ]
    | ADD `r64_rm64   -> [ 0x01 ]
    | ADD `rm64_r64   -> [ 0x03 ]
    | AND `imm32_RAX  -> [ 0x25 ]
    | AND `imm32_rm64 -> [ 0x81 ]
    | AND `imm8_rm64  -> [ 0x83 ]
    | AND `r64_rm64   -> [ 0x21 ]
    | AND `rm64_r64   -> [ 0x23 ]
    | OR  `imm32_RAX  -> [ 0x0D ]
    | OR  `imm32_rm64 -> [ 0x81 ]
    | OR  `imm8_rm64  -> [ 0x83 ]
    | OR  `r64_rm64   -> [ 0x09 ]
    | OR  `rm64_r64   -> [ 0x0B ]
    | XOR `imm32_RAX  -> [ 0x35 ]
    | XOR `imm32_rm64 -> [ 0x81 ]
    | XOR `imm8_rm64  -> [ 0x83 ]
    | XOR `r64_rm64   -> [ 0x31 ]
    | XOR `rm64_r64   -> [ 0x33 ]
    | INC -> [ 0xff ]
    | DEC -> [ 0xff ]
    | SHL `one  -> [ 0xD1 ]
    | SHL `imm8 -> [ 0xC1 ]
    | SHR `one  -> [ 0xD1 ]
    | SHR `imm8 -> [ 0xC1 ]
    | MOV `r64_rm64       -> [ 0x89 ]
    | MOV `rm64_r64       -> [ 0x8B ]
    | MOV `moffset64_RAX  -> [ 0xA1 ]
    | MOV `RAX_moffset64  -> [ 0xA3 ]
    | MOV (`imm64_r64 r321) ->
      assert (0 <= r321 && r321 <= 7);
      [ 0xB8 + r321 ]
    | MOV `imm32_rm64     -> [ 0xC7 ]
    | RET -> [ 0xc3 ]
    | SET `OF1  -> [ 0x0F; 0x90 ]
    | SET `OF0  -> [ 0x0F; 0x91 ]
    | SET `CF1  -> [ 0x0F; 0x92 ]
    | SET `CF0  -> [ 0x0F; 0x93 ]
    | SET `ZF1  -> [ 0x0F; 0x94 ]
    | SET `ZF0  -> [ 0x0F; 0x95 ]
    | SET `CF1_or_ZF1   -> [ 0x0F; 0x96 ]
    | SET `CF0_and_ZF0  -> [ 0x0F; 0x97 ]
    | SET `SF1  -> [ 0x0F; 0x98 ]
    | SET `SF0  -> [ 0x0F; 0x99 ]
    | SET `PF1  -> [ 0x0F; 0x9A ]
    | SET `PF0  -> [ 0x0F; 0x9B ]
    | SET `SF_ne_OF -> [ 0x0F; 0x9C ]
    | SET `SF_eq_OF -> [ 0x0F; 0x9D ]
    | SET `ZF1_or_SF_ne_OF  -> [ 0x0F; 0x9E ]
    | SET `ZF0_and_SF_eq_OF -> [ 0x0F; 0x9F ]
    | MOVZBQ -> [ 0x0F; 0xB6 ]
    | PUSH `rm64 -> [ 0xFF ]
    | PUSH (`r64 r321) ->
      assert (0 <= r321 && r321 <= 7);
      [ 0x50 + r321 ]
    | PUSH `imm8  -> [ 0x6A ]
    | PUSH `imm32 -> [ 0x68 ]

  let to_string_hum =
    let subvariant_to_string_hum =
      function
      | `imm32_RAX  -> "imm32 RAX"
      | `imm32_rm64 -> "imm32 rm64"
      | `imm8_rm64  -> "imm8 rm64"
      | `r64_rm64   -> "r64 rm64"
      | `rm64_r64   -> "rm64 r64"
      | `moffset64_RAX  -> "moffset64 RAX"
      | `RAX_moffset64  -> "RAX moffset64"
      | `one -> "1"
      | `imm8 -> "imm8"
      | `imm32 -> "imm32"
      | `imm64_r64 r321 -> sprintf "imm64 r64(%i)" r321
      | `r64 r321 -> sprintf "r64(%i)" r321
      | `rm64 -> "rm64"
    in
    function
    | REX fls -> sprintf "REX.%s" (RexFlags.to_string fls)
    | ModRM { mod_; reg; rm } -> sprintf "ModRM(m:%i reg:%i r/m:%i)" mod_ reg rm
    | SIB { base; index; scale } -> sprintf "SIB(s:%i i:%i b:%i)" scale index base
    | ADD sv -> "ADD " ^ subvariant_to_string_hum sv
    | AND sv -> "AND " ^ subvariant_to_string_hum sv
    | OR  sv -> "OR "  ^ subvariant_to_string_hum sv
    | XOR sv -> "XOR " ^ subvariant_to_string_hum sv
    | MOV sv -> "MOV " ^ subvariant_to_string_hum sv
    | INC -> "INC"
    | DEC -> "DEC"
    | SHL sv -> "SHL " ^ subvariant_to_string_hum sv
    | SHR sv -> "SHR " ^ subvariant_to_string_hum sv
    | RET -> "RET"
    | SET `OF1  -> "SET OF=1"
    | SET `OF0  -> "SET OF=0"
    | SET `CF1  -> "SET CF=1"
    | SET `CF0  -> "SET CF=0"
    | SET `ZF1  -> "SET ZF=1"
    | SET `ZF0  -> "SET ZF=0"
    | SET `CF1_or_ZF1   -> "SET CF=1 || ZF=1"
    | SET `CF0_and_ZF0  -> "SET CF=0 && ZF=0"
    | SET `SF1  -> "SET SF=1"
    | SET `SF0  -> "SET SF=0"
    | SET `PF1  -> "SET PF=1"
    | SET `PF0  -> "SET PF=0"
    | SET `SF_ne_OF -> "SET SF<>OF"
    | SET `SF_eq_OF -> "SET SF=OF"
    | SET `ZF1_or_SF_ne_OF  -> "SET ZF=1 || SF<>OF"
    | SET `ZF0_and_SF_eq_OF -> "SET ZF=0 && SF=OF"
    | MOVZBQ -> "MOVZBQ"
    | PUSH sv -> "PUSH " ^ subvariant_to_string_hum sv
end

module Instruction = struct
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
    | AND of binary_op
    | OR  of binary_op
    | XOR of binary_op
    | INC of Operand.t
    | DEC of Operand.t
    | SHL of Operand.t * int
    | SHR of Operand.t * int
    | MOV of binary_op
    | RET
    | SET of set_condition * Register.B8.t
    | MOVZBQ of Register.B8.t * Register.t
    | PUSH of Operand.t

  type encoded = [ `Op of Opcode.t | `LE64 of int | `LE32 of int | `I8 of int ] list

  (* This concerns the second part of ModRM "r/m" only.
   * The first part "reg" may be a register (direct), or an opcode extn. *)
  type addressing_case =
      (* Simplest case; just a register in r/m *)
    | AReg of Register.t

      (* Indirect; [r/m]. Register must not be RSP, RBP, R12, R13 *)
    | APtr of Register.t 

      (* [base + scale * index]
       * Base must not be RBP or R13.
       * Index must not be RSP. Index is optional. *)
    | ASIB of Register.t * Operand.index option

      (* [reg + disp]
       * Disp must be at most 32 bits. If possible, disp8 will be used.
       * Reg can not be RSP or R12 *)
    | ADisp of Register.t * int

      (* [base + scale * index + disp]
       * Disp must be at most 32 bits. If possible, disp8 will be used.
       * Index must not be RSP. Index is optional. *)
    | ASIB_Disp of Register.t * (Operand.index option) * int

      (* [scale * index + disp] *)
    | ASIB_no_base_Disp of Operand.index * int

      (* [RIP + disp] *)
    | ARIP_Rel of int

      (* Absolute memory location, produced with SIB byte. *)
    | ASIB_for_abs of int

  let select_addr_case =
    let module A = Operand in
    let module R = Register in
    function
    | A.Imm _ -> failwith "Can't address an immediate."
    | A.Mem64 { A.base = Some R.RIP; index = Some _; offset = _ } ->
      failwith "Can't use base-index addressing w. RIP"
    | A.Mem64 { index = Some { A.value = R.RSP; _ }; _ } ->
      failwith "RSP can't be the index register"

    | A.Reg64 reg -> AReg reg
    | A.Mem64 { A.base = Some R.RIP; index = None; offset } ->
      ARIP_Rel offset
    | A.Mem64 { A.base = None; index = None; offset } ->
      ASIB_for_abs offset

    | A.Mem64 { A.base = Some base; index = None; offset } ->
      begin
        match base with
        | RSP | R12 ->
          if offset = 0
          then ASIB (base, None)
          else ASIB_Disp (base, None, offset)
        | RBP | R13 ->
          ADisp (base, offset)
        | base ->
          if offset = 0
          then APtr base
          else ADisp (base, offset)
      end

    | A.Mem64 { A.base = Some base; index = Some index; offset } ->
      begin
        match base with
        | RBP | R13 ->
          ASIB_Disp (base, Some index, offset)
        | base ->
          if offset = 0
          then ASIB (base, Some index)
          else ASIB_Disp (base, Some index, offset)
      end

    | A.Mem64 { A.base = None; index = Some index; offset } ->
      ASIB_no_base_Disp (index, offset)

  type encoded_operands =
    { rex_flags : Opcode.RexFlags.t
    ; suffix : encoded 
    }
  
  let split_4th x =
    assert (0 <= x && x <= 15);
    (x land 0b1000 <> 0, x land 0b0111)

  (* returns the rex flags required and the suffix *)
  let encode_operands 
      (one : [ `Op_extn of int | `Reg of Register.t ])
      (two : addressing_case)
      : encoded_operands
    =
    let module A = Operand in
    let module C = Opcode in
    let module R = Register in
    let rex_flags = C.RexFlags.empty in
    let (rex_flags, modrm_r) =
      match one with
      | `Op_extn i ->
        assert (i >= 0 && i <= 7);
        (rex_flags, i)
      | `Reg i ->
        let r4, r321 = split_4th (Register.operand_number i) in
        (C.RexFlags.set ~r:r4 rex_flags, r321)
    in
    let make_modrm ~mod_ ~rm =
      `Op (C.ModRM { C.mod_; reg = modrm_r; rm })
    in
    let make_sib rex_flags base index ~base101 =
      let (b4, b321) =
        match base with
        | Some reg -> split_4th (R.operand_number reg)
        | None -> (false, 0b101)
      in
      begin
        match base101 with
        | `Demand -> assert (b321 = 0b101)
        | `Allow  -> ()
        | `Forbid -> assert (b321 <> 0b101)
      end;
      let (i4, i321) =
        match index with
        | Some { A.value = reg; _ } ->
          let (x, y) = split_4th (R.operand_number reg) in
          assert (y <> 0b100);
          (x, y)
        | None -> (false, 0b100)
      in
      let s2 =
        let scale =
          match index with
          | None -> 1
          | Some { A.scale; _ } -> scale
        in
        match scale with
        | 1 -> 0b00
        | 2 -> 0b01
        | 4 -> 0b10
        | 8 -> 0b11
        | _ -> failwith "attempted to encode scale <> 1, 2, 4, 8"
      in
      ( C.RexFlags.set ~b:b4 ~x:i4 rex_flags
      , C.SIB { C.base = b321; index = i321; scale = s2 }
      )
    in
    let disp_mod_data offset =
      match immediate_size offset with
      | `Zero  -> (0b01, `I8 0)
      | `I8 i  -> (0b01, `I8 i)
      | `I32 i -> (0b10, `LE32 i)
      | `I63 _ -> failwith "addressing offset too large"
    in
    match two with
    | AReg reg ->
      let b4, b321 = split_4th (R.operand_number reg) in
      { rex_flags = C.RexFlags.set ~b:b4 rex_flags
      ; suffix = [ make_modrm ~mod_:0b11 ~rm:b321 ]
      }

    | APtr (R.RSP | R.RBP | R.R12 | R.R13 | R.RIP) ->
      failwith "attempted to encode [r/m] with invalid reg"
    | APtr reg ->
      let b4, b321 = split_4th (R.operand_number reg) in
      assert (b321 <> 0b100 && b321 <> 0b101);
      { rex_flags = C.RexFlags.set ~b:b4 rex_flags
      ; suffix = [ make_modrm ~mod_:0b00 ~rm:b321 ]
      }

    | ASIB ((R.RBP | R.R13), _) ->
      failwith "attempted to encode SIB with invalid base"
    | ASIB (_, Some { A.value = R.RSP; _ }) ->
      failwith "attempted to encode SIB with index RSP"
    | ASIB (base, index) ->
      let mrm = make_modrm  ~mod_:0b00 ~rm:0b100 in
      let (rex_flags, sib) = make_sib rex_flags (Some base) index ~base101:`Forbid in
      { rex_flags; suffix = [ mrm; `Op sib ] }

    | ADisp ((R.RSP | R.R12), _) ->
      failwith "attempted to encode [r/m+disp] with invalid reg"
    | ADisp (reg, offset) ->
      let (b4, b321) = split_4th (R.operand_number reg) in
      assert (b321 <> 0b100);
      let (mod_, data) = disp_mod_data offset in
      { rex_flags = C.RexFlags.set ~b:b4 rex_flags
      ; suffix = [ make_modrm ~mod_ ~rm:b321; data ]
      }

    | ASIB_Disp (_, Some { A.value = R.RSP; _ }, _) ->
      failwith "attempted to encode SIB_Disp with index RSP"
    | ASIB_Disp (base, index, offset) ->
      let (mod_, data) = disp_mod_data offset in
      let mrm = make_modrm  ~mod_ ~rm:0b100 in
      let (rex_flags, sib) = make_sib rex_flags (Some base) index ~base101:`Allow in
      { rex_flags; suffix = [ mrm; `Op sib; data ] }

    | ASIB_for_abs addr when not (is_int32 addr) ->
      failwith "address in absolute memory reference is too long"
    | ASIB_for_abs addr ->
      let mrm = make_modrm  ~mod_:0b00 ~rm:0b100 in
      let sib = C.SIB { C.base = 0b101; index = 0b100; scale = 0b00 } in
      { rex_flags; suffix = [ mrm; `Op sib; `LE32 addr ] }

    | ASIB_no_base_Disp ({ A.value = R.RSP; _ }, _) ->
      failwith "attempted to encode SIB_no_base_Disp with index RSP"
    | ASIB_no_base_Disp (index, offset) ->
      let mrm = make_modrm ~mod_:0b00 ~rm:0b100 in
      let (rex_flags, sib) = make_sib rex_flags None (Some index) ~base101:`Demand in
      { rex_flags; suffix = [ mrm; `Op sib; `LE32 offset ] }

    | ARIP_Rel addr when not (is_int32 addr) ->
      failwith "address in rip-relative memory reference is too long"
    | ARIP_Rel addr ->
      { rex_flags
      ; suffix = [ make_modrm ~mod_:0b00 ~rm:0b101; `LE32 addr ]
      }

  let make_instruction
      opcode
      ?(rex_w=true)
      ?modrm_sib_disp
      ?(data=[])
      ()
    =
    let module C = Opcode in
    let { rex_flags; suffix } =
      match modrm_sib_disp with
      | Some (one, two) ->
        let two = select_addr_case two in
        encode_operands one two
      | None ->
        { rex_flags = C.RexFlags.empty; suffix = [] }
    in
    let rex_flags = C.RexFlags.set ~w:rex_w rex_flags in
    let instruction =
      (`Op opcode :: suffix) @ data
    in
    let instruction =
      if C.RexFlags.is_empty rex_flags
      then instruction
      else `Op (C.REX rex_flags) :: instruction
    in
    instruction

  (* Most bops follow this pattern
   * BOP imm32 RAX           C1 id
   * BOP imm32 reg/mem64     C2 /extn id
   * BOP imm8  reg/mem64     C3 /extn ib
   * BOP reg64 reg/mem64     C4 /r
   * BOP reg64/mem reg64     C5 /r *)
  let typical_binary_op opcode_constr ~opcode_extn =
    let module A = Operand in
    let module R = Register in
    function
    | { source = _; dest = A.Imm _ } ->
      failwith "Immediate can't be the dest of a BOP"
    | { source = A.Mem64 _; dest = A.Mem64 _ } ->
      failwith "BOP can't have two memory operands"
    | { source = A.Imm imm; dest } ->
      let (subvariant, data) =
        match immediate_size imm with
        | `Zero   -> (`imm8_rm64,  [ `I8 0 ])
        | `I8 i   -> (`imm8_rm64,  [ `I8 i ])
        | `I32 i  -> (`imm32_rm64, [ `LE32 i ])
        | `I63 _  -> failwith "Immediate too large for BOP"
      in
      begin
        (* Optimisation: 05 id is a shorter version of 83 /0,RAX id *)
        match (dest, subvariant) with
        | (A.Reg64 R.RAX, `imm32_rm64) ->
          make_instruction (opcode_constr `imm32_RAX) ~data ()
        | (dest, _) ->
          make_instruction
            (opcode_constr subvariant)
            ~modrm_sib_disp:(`Op_extn opcode_extn, dest)
            ~data
            ()
      end
    | { source = A.Reg64 src; dest } ->
      make_instruction (opcode_constr `r64_rm64) ~modrm_sib_disp:(`Reg src, dest) ()
    | { source = src; dest = A.Reg64 dest } ->
      make_instruction (opcode_constr `rm64_r64) ~modrm_sib_disp:(`Reg dest, src) ()

  let parts =
    let module A = Operand in
    let module C = Opcode in
    let module R = Register in
    function
    | ADD sd -> typical_binary_op (fun x -> C.ADD x) ~opcode_extn:0 sd
    | AND sd -> typical_binary_op (fun x -> C.AND x) ~opcode_extn:4 sd
    | OR  sd -> typical_binary_op (fun x -> C.OR  x) ~opcode_extn:1 sd
    | XOR sd -> typical_binary_op (fun x -> C.XOR x) ~opcode_extn:6 sd

    | INC (A.Imm _) ->
      failwith "Can't increment an immediate"
    | INC tgt ->
      make_instruction C.INC ~modrm_sib_disp:(`Op_extn 0, tgt) ()

    | DEC (A.Imm _) ->
      failwith "Can't decrement an immediate"
    | DEC tgt ->
      make_instruction C.DEC ~modrm_sib_disp:(`Op_extn 1, tgt) ()

    | SHL (A.Imm _, _) ->
      failwith "Can't shift-left an immediate"
    | SHL (_, i) when i <= 0 || i >= 64 ->
      failwith "Shift left: invalid # bits"
    | SHL (tgt, 1) ->
      make_instruction (C.SHL `one)  ~modrm_sib_disp:(`Op_extn 4, tgt) ()
    | SHL (tgt, bts) ->
      make_instruction (C.SHL `imm8) ~modrm_sib_disp:(`Op_extn 4, tgt) ~data:[ `I8 bts ] ()

    | SHR (A.Imm _, _) ->
      failwith "Can't shift-left an immediate"
    | SHR (_, i) when i <= 0 || i >= 64 ->
      failwith "Shift left: invalid # bits"
    | SHR (tgt, 1) ->
      make_instruction (C.SHR `one)  ~modrm_sib_disp:(`Op_extn 5, tgt) ()
    | SHR (tgt, bts) ->
      make_instruction (C.SHR `imm8) ~modrm_sib_disp:(`Op_extn 5, tgt) ~data:[ `I8 bts ] ()

    | MOV { source = _; dest = A.Imm _ } ->
      failwith "Immediate can't be the dest of an MOV"
    | MOV { source = A.Mem64 _; dest = A.Mem64 _ } ->
      failwith "MOV can't have two memory operands"
    | MOV { source = A.Imm imm; dest } when is_int32 imm ->
      make_instruction
        (C.MOV `imm32_rm64)
        ~modrm_sib_disp:(`Op_extn 0, dest)
        ~data:[ `LE32 imm ]
        ();
    | MOV { source = A.Imm _; dest = A.Mem64 _ } ->
      failwith "MOV immediate too large for dest to be memory"
    | MOV { source = A.Imm imm; dest = A.Reg64 dest_reg } ->
      let b4, b321 = split_4th (R.operand_number dest_reg) in
      [ `Op (C.REX C.RexFlags.(set ~w:true ~b:b4 empty))
      ; `Op (C.MOV (`imm64_r64 b321))
      ; `LE64 imm
      ]
    | MOV { source = A.Reg64 src; dest } ->
      make_instruction (C.MOV `r64_rm64) ~modrm_sib_disp:(`Reg src, dest) ()
    | MOV { source = src; dest = A.Reg64 dest } ->
      make_instruction (C.MOV `rm64_r64) ~modrm_sib_disp:(`Reg dest, src) ()

    | RET ->
      (* RET does not need REX.W; it defaults to 64 bits *)
      [ `Op C.RET ]

    | SET (cond, tgt) ->
      let rm = R.B8.operand_number tgt in
      [ `Op (C.SET cond)
      ; `Op (C.ModRM { C.mod_ = 0b11; reg = 0; rm })
      ]

    | MOVZBQ (source, dest) ->
      let rm = R.B8.operand_number source in
      let r4, r321 = split_4th (R.operand_number dest) in
      [ `Op (C.REX C.RexFlags.(set ~w:true ~r:r4 empty))
      ; `Op C.MOVZBQ
      ; `Op (C.ModRM { C.mod_ = 0b11; reg = r321; rm })
      ]

    | PUSH (A.Imm imm) ->
      begin
        let imm = 
          match immediate_size imm with
          | `Zero -> `I8 0
          | `I8 i -> `I8 i
          | `I32 i -> `I32 i
          | `I63 _ -> failwith "PUSH immediate too large"
        in
        match imm with
        | `I8  i -> [ `Op (C.PUSH `imm8);  `I8 i   ]
        | `I32 i -> [ `Op (C.PUSH `imm32); `LE32 i ]
      end
    | PUSH (A.Reg64 src) ->
      let r4, r321 = split_4th (R.operand_number src) in
      let i = `Op (C.PUSH (`r64 r321)) in
      if r4
      then [ `Op (C.REX C.RexFlags.(set ~b:true empty)); i ]
      else [ i ]
    | PUSH (A.Mem64 src) ->
      make_instruction
        ~rex_w:false (C.PUSH `rm64)
        ~modrm_sib_disp:(`Op_extn 6, A.Mem64 src)
        ()

  let assemble_into t buf =
    List.iter (parts t) ~f:(
      function
      | `Op op  -> List.iter (Opcode.to_ints op) ~f:(Iobuf.Fill.int8 buf)
      | `LE32 v -> Iobuf.Fill.int32_le buf v
      | `LE64 v -> Iobuf.Fill.int64_le buf v
      | `I8 v   -> Iobuf.Fill.int8     buf v
    )

  let to_string_assembled t =
    let buf = Iobuf.create ~len:32 in
    assemble_into t buf;
    Iobuf.flip_lo buf;
    Iobuf.to_string buf

  let t_list_to_string_assembled ts =
    let chunk_size = Int.min (List.length ts * 32) 10024 in
    let chunks_rev = ref [] in
    let new_chunk () =
      let c = Iobuf.create ~len:chunk_size in
      chunks_rev := c :: !chunks_rev;
      c
    in
    List.iter ts ~f:(fun t ->
      let chunk =
        match !chunks_rev with
        | [] -> new_chunk ()
        | x::_ when Iobuf.length x < 32 -> new_chunk ()
        | x::_ -> x
      in
      assemble_into t chunk
    );

    List.iter !chunks_rev ~f:Iobuf.flip_lo;
    let len =
      List.fold !chunks_rev ~init:0 ~f:(fun acc chunk -> acc + Iobuf.length chunk)
    in

    let final = Iobuf.create ~len in
    List.iter
      (List.rev !chunks_rev)
      ~f:(fun c -> Iobuf.transfer ?len:None ~src:c ~dst:final);
    Iobuf.flip_lo final;
    Iobuf.to_string final

  let set_condition_to_string_gas =
    function
    | `OF1  -> "o"
    | `OF0  -> "no"
    | `CF1  -> "c"
    | `CF0  -> "nc"
    | `ZF1  -> "z"
    | `ZF0  -> "nz"
    | `CF1_or_ZF1   -> "be"
    | `CF0_and_ZF0  -> "a"
    | `SF1  -> "s"
    | `SF0  -> "ns"
    | `PF1  -> "p"
    | `PF0  -> "np"
    | `SF_ne_OF -> "l"
    | `SF_eq_OF -> "ge"
    | `ZF1_or_SF_ne_OF  -> "le"
    | `ZF0_and_SF_eq_OF -> "g"

  let to_string_gas =
    let bop mnemonic { source; dest } =
      sprintf "%s %s,%s" mnemonic (Operand.to_string_gas source) (Operand.to_string_gas dest)
    in
    function
    | ADD args -> bop "addq" args
    | AND args -> bop "andq" args
    | OR  args -> bop "orq"  args
    | XOR args -> bop "xorq" args
    | INC tgt -> sprintf "incq %s" (Operand.to_string_gas tgt)
    | DEC tgt -> sprintf "decq %s" (Operand.to_string_gas tgt)
    | SHL (tgt, bts) -> sprintf "shlq $%i,%s" bts (Operand.to_string_gas tgt)
    | SHR (tgt, bts) -> sprintf "shrq $%i,%s" bts (Operand.to_string_gas tgt)
    | MOV args -> bop "movq" args
    | RET -> "ret"
    | SET (cond, tgt) ->
      sprintf "set%s %s"
        (set_condition_to_string_gas cond)
        (Register.B8.to_string_gas tgt)
    | MOVZBQ (src, dest) ->
      sprintf "movzbq %s,%s" (Register.B8.to_string_gas src) (Register.to_string_gas dest)
    | PUSH src -> "pushq " ^ Operand.to_string_gas src
end

module Std = struct
  module R = Register
  module A = Operand
  module I = Instruction
  let assemble = I.t_list_to_string_assembled
end
