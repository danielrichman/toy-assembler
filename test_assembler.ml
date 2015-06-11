open Core.Std
open Core_extended.Std
open Common

let operands =
  let open Assembler.Register in
  let open Assembler.Operand in
  [ Imm 8
  ; Imm 123123123
  ; Imm 1058901839296
  ; Imm (-10)
  ; Imm (-1289458)
  ; Imm (-1058901839296)

  ; Reg64 RAX
  ; Reg64 R9
  ; Reg64 RSP
  ; Reg64 RBP
  ; Reg64 R13
  ; Reg64 R12

  ; mem ~base:RBX ()
  ; mem ~base:R15 ()
  ; mem ~base:RSP ()
  ; mem ~base:R12 ()
  ; mem ~base:RBP ()
  ; mem ~base:R13 ()

  ; mem ~base:RDX ~offset:(-10)     ()
  ; mem ~base:R8  ~offset:10        ()
  ; mem ~base:RSP ~offset:456456    ()
  ; mem ~base:R12 ~offset:(-10)     ()
  ; mem ~base:RBP ~offset:(-456456) ()
  ; mem ~base:R13 ~offset:10        ()

  ; mem ~base:RDI ~index:(R15,1) ()
  ; mem ~base:R10 ~index:(R14,2) ()
  ; mem ~base:RSP ~index:(RCX,4) ()
  ; mem ~base:R12 ~index:(RSI,8) ()
  ; mem ~base:RBP ~index:(RDI,1) ()
  ; mem ~base:R13 ~index:(RBP,2) ()
  
  ; mem ~base:RDI ~index:(R15,4) ~offset:(-10)     ()
  ; mem ~base:R10 ~index:(R14,8) ~offset:10        ()
  ; mem ~base:RSP ~index:(RCX,1) ~offset:456456    ()
  ; mem ~base:R12 ~index:(RSI,2) ~offset:(-10)     ()
  ; mem ~base:RBP ~index:(RDI,4) ~offset:(-456456) ()
  ; mem ~base:R13 ~index:(RBP,8) ~offset:10        ()

  ; mem ~index:(R9, 1) ~offset:(-10)     ()
  ; mem ~index:(RSI,2) ~offset:10        ()
  ; mem ~index:(RBP,4) ~offset:456456    ()
  ; mem ~index:(RSI,8)                   ()
  ; mem ~index:(R13,1)                   ()

  ; mem ~base:RIP                  ()
  ; mem ~base:RIP ~offset:12391203 ()
  ; mem ~base:RIP ~offset:(-10)    ()

  ; mem ~offset:12837192 ()
  ; mem ~offset:100      ()
  ]

let operand_pairs ~f =
  List.concat_map operands ~f:(fun source ->
    List.filter_map operands ~f:(fun dest ->
      let p = (source, dest) in
      if f p then Some p else None
    )
  )

let instructions =
  [ (* ADD *)
    begin
      let args = operand_pairs ~f:(
        function
        | (_, Imm _) -> false
        | (Mem64 _, Mem64 _) -> false
        | (Imm i, _) -> is_int32 i
        | _ -> true
      )
      in
      List.map args ~f:(fun (source, dest) ->
        Assembler.Instruction.ADD { source; dest }
      )
    end

  ; (* INC *)
    List.filter_map operands ~f:(fun tgt ->
      match tgt with
      | Imm _ -> None
      | tgt -> Some (Assembler.Instruction.INC tgt)
    )

  ; (* DEC *)
    List.filter_map operands ~f:(fun tgt ->
      match tgt with
      | Imm _ -> None
      | tgt -> Some (Assembler.Instruction.DEC tgt)
    )

  ; (* MOV *)
    begin
      let args = operand_pairs ~f:(
        function
        | (_, Imm _) -> false
        | (Mem64 _, Mem64 _) -> false
        | (Imm _, Reg64 _) -> true
        | (Imm i, _) -> is_int32 i
        | _ -> true
      )
      in
      List.map args ~f:(fun (source, dest) ->
        Assembler.Instruction.MOV { source; dest }
      )
    end

  ; (* RET *)
    [ Assembler.Instruction.RET ]
  ]
  |> List.concat

let with_temp_file ~suffix ~f =
  let (fn, chan) = Filename.open_temp_file "test_assembler." suffix in
  protect ~finally:(fun () -> Sys.remove fn)          ~f:(fun () ->
  protect ~finally:(fun () -> Out_channel.close chan) ~f:(fun () ->
    f fn chan
  ))

let gnu_as instructions =
  with_temp_file ~suffix:".asm.s" ~f:(fun asm_in_fn asm_channel ->
  with_temp_file ~suffix:".a.out" ~f:(fun a_out _ ->
    instructions
    |> List.map ~f:Assembler.Instruction.to_string_gas
    |> Out_channel.output_lines asm_channel;
    Out_channel.close asm_channel;

    Shell.run "as" ["-o"; a_out; asm_in_fn];
    Shell.run_full "objcopy" [a_out; "-O"; "binary"; "/dev/stdout"]
  ))

let objdump assembled =
  with_temp_file ~suffix:".bin" ~f:(fun bin_fn bin_channel ->
  with_temp_file ~suffix:".o" ~f:(fun elf_fn _ ->
    Out_channel.output_string bin_channel assembled;
    Out_channel.close bin_channel;

    Shell.run "objcopy"
      [ "-I"; "binary"; bin_fn
      ; "--rename-section"; ".data=.text,contents,alloc,load,code"
      ; "-B"; "i386:x86-64"; "-O"; "elf64-x86-64"; elf_fn
      ];
    Shell.run_full "objdump" ["-d"; elf_fn]
  ))

let hexdump s =
  let base_0 = Char.to_int '0' in
  let base_A = Char.to_int 'a' in
  let digit x =
    if x < 10
    then Char.of_int_exn (base_0 + x)
    else Char.of_int_exn (base_A + x - 10)
  in
  String.init (String.length s * 2) ~f:(fun i ->
    let c = Char.to_int (s.[i / 2]) in
    let d =
      if i land 1 = 0
      then c lsr 4
      else c land 15
    in
    digit d
  )

let debug inst =
  let gas_binary = gnu_as [inst] in
  let assembled = Assembler.Instruction.to_string_assembled inst in
  let disassembled = objdump assembled in
  let parts = Assembler.Instruction.parts inst in

  printf "GAS:        %s\n" (Assembler.Instruction.to_string_gas inst);
  printf "AS output:  %s\n" (hexdump gas_binary);
  printf "Assembled:  %s\n" (hexdump assembled);
  printf "Parts:\n";
  List.iter parts ~f:(function
    | `Op op  -> printf "    %s\n" (Assembler.Opcode.to_string_hum op)
    | `LE32 i -> printf "    LE32 %i\n" i
    | `LE64 i -> printf "    LE64 %i\n" i
    | `I8 i   -> printf "    I8 %i\n" i
  );
  printf "Disassembly\n%s"  disassembled

let compare_with_gas instructions =
  let gas_binary = gnu_as instructions in
  let assembled = Assembler.Instruction.t_list_to_string_assembled instructions in
  gas_binary = assembled

let rec bisect =
  function
  | [] -> Ok ()
  | [one] ->
    if compare_with_gas [one]
    then Ok ()
    else Error one
  | many ->
    if compare_with_gas many
    then Ok ()
    else begin
      let l, r = List.split_n many (List.length many / 2) in
      Result.bind (bisect l) (fun () -> bisect r)
    end

module Counts = struct
  include Map.Make(struct
    type t = [ `ADD | `INC | `DEC | `MOV | `RET ] with compare, sexp
  end)

  let key_of_instruction =
    let open Assembler.Std in
    function
    | I.ADD _ -> `ADD
    | I.INC _ -> `INC
    | I.DEC _ -> `DEC
    | I.MOV _ -> `MOV
    | I.RET -> `RET

  let key_to_string =
    function
    | `ADD -> "ADD"
    | `INC -> "INC"
    | `DEC -> "DEC"
    | `MOV -> "MOV"
    | `RET -> "RET"

  let count_instructions =
    List.fold ~init:empty ~f:(fun acc inst ->
      Map.change acc (key_of_instruction inst) (fun c ->
        Some ((Option.value c ~default:0) + 1)
      )
    )
end

let () =
  match bisect instructions with
  | Ok () ->
    Map.iter
      (Counts.count_instructions instructions)
      ~f:(fun ~key ~data ->
        printf "%-5s %-3i OK\n" (Counts.key_to_string key) data
      )
  | Error first ->
    debug first;
    failwith "Disagreed with GNU AS"
