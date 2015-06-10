open Core.Std

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
end

module Operand = struct
  type index =
    { value : Register.t
    ; scale : int
    }

  type regmem =
    { base : Register.t option
    ; index : index option
    ; offset : int
    }

  type t =
    | Imm   of int
    | Reg64 of Register.t
    | Mem64 of regmem

  let regmem ?base ?index ?(offset=0) () =
    let index = Option.map index ~f:(fun (a, b) -> { value = a; scale = b }) in
    { base; index; offset }
end

(* XXX I'm not sure this should really be called 'Opcode' *)
module Opcode = struct
  module RexFlags : sig
    type t = private int
    val empty : t
    val set : ?w:bool -> ?r:bool -> ?x:bool -> ?b:bool -> t -> t
    val to_int : t -> int
    val is_empty : t -> bool
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
  end

  type modrm = { mod_ : int; reg : int; rm : int }
  type sib = { base : int; index : int; scale : int }

  type t =
    | REX of RexFlags.t
    | ModRM of modrm
    | SIB of sib
    | ADD of [ `imm32 | `rm64_imm32 | `rm64_imm8 | `rm64_r64 | `r64_rm64 ]

  let to_int = function
    | REX fls -> 0x40 lor (RexFlags.to_int fls)
    | ModRM { mod_; reg; rm } ->
      assert (0 <= mod_ && 3 <= mod_);
      assert (0 <= reg  && 7 <= reg);
      assert (0 <= rm   && 7 <= rm);
      (mod_ lsl 5) lor (reg  lsl 3) lor rm
    | SIB { base; index; scale } ->
      assert (0 <= base  && base <= 7);
      assert (0 <= index && index <= 7);
      assert (0 <= scale && scale <= 3);
      (scale lsl 5) lor (index lsl 3) lor base
    | ADD `imm32      -> 05
    | ADD `rm64_imm32 -> 81
    | ADD `rm64_imm8  -> 83
    | ADD `rm64_r64   -> 01
    | ADD `r64_rm64   -> 03

  let to_char = Fn.compose Char.of_int_exn to_int
end

module Instruction = struct
  type add = { source : Operand.t; dest : Operand.t }

  type t =
    | ADD of add

  type encoded = [ `Op of Opcode.t | `LE32 of int | `I8 of int ] list

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

  (* This concerns the second part of ModRM "r/m" only.
   * The first part "reg" may be a register (direct), or an opcode extn. *)
  type addressing_case =
      (* Simplest case; just a register in r/m *)
    | AReg of Register.t

      (* Indirect; [r/m]. Register must not be RSP, RBP, R12, R13 *)
    | APtr of Register.t 

      (* [base + scale * index]
       * Base must not be RBP or R13. Base is _not_ optional.
       * Index must not be RSP. Index is optional. *)
    | ASIB of Register.t * Operand.index option

      (* [reg + disp]
       * Disp must be at most 32 bits. If possible, disp8 will be used.
       * Reg can not be RSP or R12 *)
    | ADisp of Register.t * int

      (* [base + scale * index + disp]
       * Disp must be at most 32 bits. If possible, disp8 will be used.
       * Base is _not_ optional.
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
    assert (0 <= x && x <= 7);
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

  let parts =
    let module A = Operand in
    let module C = Opcode in
    let module R = Register in
    function
    | ADD { source = _; dest = A.Imm _ } ->
      failwith "Immediate can't be the dest of an ADD"
    | ADD { source = A.Mem64 _; dest = A.Mem64 _ } ->
      failwith "Can't have two memory operands"
    | ADD { source = A.Imm imm; dest } ->
      let (subvariant, data, smaller_than_32) =
        match immediate_size imm with
        | `Zero   -> (`rm64_imm8, [ `I8 0 ], true)
        | `I8 i   -> (`rm64_imm8, [ `I8 i ], true)
        | `I32 i  -> (`rm64_imm32, [ `LE32 i ], true)
        | `I63 _  -> failwith "Immediate too large for ADD"
      in
      begin
        match (dest, smaller_than_32) with
        | (A.Reg64 R.RAX, true) ->
          make_instruction (C.ADD `imm32) ~data:[ `LE32 imm ] ()
        | (dest, _) ->
          make_instruction (C.ADD subvariant) ~modrm_sib_disp:(`Op_extn 0, dest) ~data ()
      end
    | ADD { source = src; dest = A.Reg64 dest } ->
      make_instruction (C.ADD `r64_rm64) ~modrm_sib_disp:(`Reg dest, src) ()
    | ADD { source = A.Reg64 src; dest } ->
      make_instruction (C.ADD `rm64_r64) ~modrm_sib_disp:(`Reg src, dest) ()

  let encode_into t buf =
    List.iter (parts t) ~f:(
      function
      | `Op op  -> Iobuf.Poke.char     buf (Opcode.to_char op) ~pos:0
      | `LE32 v -> Iobuf.Poke.int32_le buf v ~pos:0
      | `I8 v   -> Iobuf.Poke.int8     buf v ~pos:0
    )
end
