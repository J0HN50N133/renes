open Cpu
open Instruction
open Belt

let hexrep = i => i->Js.Int.toStringWithRadix(~radix=16)
let bindump = i => i->Js.Int.toStringWithRadix(~radix=2)
type l_or_r = L | R
let padding = (~l_or_r=R, s, w, c) => {
  let lack = Js.Math.max_int(w - s->Js.String2.length, 0)
  switch l_or_r {
  | R => s ++ Js.String2.repeat(c, lack)
  | L => Js.String2.repeat(c, lack) ++ s
  }
}
let padding_with_0 = (s, w) => {
  let lack = Js.Math.max_int(w - s->Js.String2.length, 0)
  Js.String2.repeat("0", lack) ++ s
}

type info = {
  mutable pc: string,
  mutable a: string,
  mutable x: string,
  mutable y: string,
  mutable op_code: string,
  mutable status: string,
}
exception RunTooMuch
let count = ref(0)
let debug_limit = (cpu: Cpu.cpu, limit: int) => {
  let i = {
    pc: "",
    a: "",
    x: "",
    y: "",
    op_code: "",
    status: "",
  }
  if limit == -1 || count.contents < limit {
    let pc = cpu.pc
    let pchex = "0x" ++ pc->hexrep
    let instruction = switch HashMap.get(instruction_table, Cpu.mem_read(cpu, pc)) {
    | Some(ins) => "0x" ++ ins.bin->hexrep
    | None => "Decode Error"
    }
    let status = Cpu.status_2_vector(cpu)->bindump
    let status = Js.String.repeat(8 - String.length(status), "0") ++ status
    i.pc = pchex
    i.a = "0x" ++ cpu.register_a->hexrep
    i.x = "0x" ++ cpu.register_x->hexrep
    i.y = "0x" ++ cpu.register_y->hexrep
    i.op_code = instruction
    i.status = status
    Js.log(i)
    count := count.contents + 1
  }
}
let debug = (cpu: Cpu.cpu) => cpu->debug_limit(-1)

let trace = (cpu: Cpu.cpu) => {
  let code = cpu->Cpu.mem_read(cpu.pc)
  let ops = instruction_table->HashMap.get(code)->Option.getExn
  let begin = cpu.pc
  let hexdump = ref([])
  let _ = hexdump.contents->Js.Array2.push(code)
  let (mem_addr, stored_value) = switch ops.mode {
  | Immediate | NoneAddressing => (0, 0)
  | _ => {
      let addr = cpu->Cpu.get_absolute_addr(ops.mode, begin + 1)
      (addr, cpu->Cpu.mem_read(addr))
    }
  }
  let f2 = addr => addr->hexrep->padding_with_0(2)
  let f4 = addr => addr->hexrep->padding_with_0(4)
  let tmp = switch ops.len {
  | 1 =>
    switch ops.bin {
    | 0x0A | 0x4A | 0x2A | 0x6A => "A "
    | _ => ""
    }
  | 2 => {
      let addr = cpu->Cpu.mem_read(begin + 1)
      let _ = hexdump.contents->Js.Array2.push(addr)
      switch ops.mode {
      | Immediate => `#$${f2(addr)}`
      | ZeroPage => `$${f2(addr)} = ${f2(stored_value)}`
      | ZeroPage_X => `$${f2(addr)},X @ ${f2(mem_addr)} = ${f2(stored_value)}`
      | ZeroPage_Y => `$${f2(addr)},Y @ ${f2(mem_addr)} = ${f2(stored_value)}`
      | Indirect_X =>
        `($${f2(addr)},X) @ ${(addr + cpu.register_x)->mod(0x100)->f2} = ${mem_addr
          ->hexrep
          ->padding_with_0(4)} = ${f2(stored_value)}`
      | Indirect_Y =>
        `($${f2(addr)}),Y = ${(mem_addr - cpu.register_y)
          ->land(0xFFFF)
          ->f4} @ ${mem_addr->f4} = ${f2(stored_value)}`
      | Relative =>
        `$${f4(
            mod(
              begin +
              if addr < 0x80 {
                addr
              } else {
                addr - 256
              } + 2,
              0x10000,
            ),
          )}`
      | _ => failwith("Unexpected")
      }
    }
  | 3 => {
      let addr_lo = cpu->Cpu.mem_read(begin + 1)
      let addr_hi = cpu->Cpu.mem_read(begin + 2)
      let _ = hexdump.contents->Js.Array2.push(addr_lo)
      let _ = hexdump.contents->Js.Array2.push(addr_hi)
      let addr = cpu->Cpu.mem_read_2bytes(begin + 1)
      switch ops.mode {
      | Indirect => {
          let jmp_addr = if land(addr, 0x00FF) === 0x00FF {
            let lo = cpu->Cpu.mem_read(addr)
            let hi = cpu->Cpu.mem_read(land(addr, 0xff00))
            lsl(hi, 8)->lor(lo)
          } else {
            cpu->Cpu.mem_read_2bytes(addr)
          }
          `($${f4(addr)}) = ${jmp_addr->hexrep->padding_with_0(4)}`
        }
      | NoneAddressing => `$${f4(addr)}`
      | Absolute =>
        switch ops.code {
        | JMP | JSR => `$${f4(addr)}`
        | _ => `$${f4(addr)} = ${f2(stored_value)}`
        }
      | Absolute_X => `$${f4(addr)},X @ ${f4(mem_addr)} = ${f2(stored_value)}`
      | Absolute_Y => `$${f4(addr)},Y @ ${f4(mem_addr)} = ${f2(stored_value)}`
      | _ => failwith("unexpected addressing mode")
      }
    }
  | _ => failwith("unknown op length")
  }
  let hex_str = hexdump.contents->Belt.Array.joinWith(" ", f2)
  let asm_str =
    `${f4(begin)}  ${hex_str->padding(8, " ")} ${Instruction.string_of_opcode(ops)->padding(
        ~l_or_r=L,
        _,
        4,
        " ",
      )} ${tmp}`->Js.String2.trim

  (asm_str->padding(47, " ") ++
  " A:" ++
  cpu.register_a->hexrep->padding_with_0(2) ++
  " X:" ++
  cpu.register_x->hexrep->padding_with_0(2) ++
  " Y:" ++
  cpu.register_y->hexrep->padding_with_0(2) ++
  " P:" ++
  cpu->Cpu.status_2_vector->hexrep->padding_with_0(2) ++
  " SP:" ++
  cpu.stack_pointer->hexrep->padding_with_0(2) ++ "\n")->Js.String2.toUpperCase
}
