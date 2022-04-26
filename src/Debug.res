open Cpu
open Instruction

let hexdump = i => i->Js.Int.toStringWithRadix(~radix=16)
let bindump = i => i->Js.Int.toStringWithRadix(~radix=2)
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
    let pchex = "0x" ++ pc->hexdump
    let instruction = switch Belt.HashMap.get(
      Instruction.instruction_table,
      Cpu.mem_read(cpu, pc),
    ) {
    | Some(ins) => "0x" ++ ins.bin->hexdump
    | None => "Decode Error"
    }
    let status = Cpu.status_2_vector(cpu)->bindump
    let status = Js.String.repeat(8 - String.length(status), "0") ++ status
    i.pc = pchex
    i.a = "0x" ++ cpu.register_a->hexdump
    i.x = "0x" ++ cpu.register_x->hexdump
    i.y = "0x" ++ cpu.register_y->hexdump
    i.op_code = instruction
    i.status = status
    Js.log(i)
    count := count.contents + 1
  }
}
let debug = (cpu: Cpu.cpu) => cpu->debug_limit(-1)
