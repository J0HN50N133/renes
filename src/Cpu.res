/*
 * status register  Flags (bit 7 to bit 0)
 * N	Negative
 * V	Overflow
 * -	ignored
 * B	Break
 * D	Decimal (use BCD for arithmetics)
 * I	Interrupt (IRQ disable)
 * Z	Zero
 * C	Carry
 */
type cpu = {
  mutable register_a: int,
  mutable register_x: int,
  mutable register_y: int,
  mutable stack_pointer: int,
  mutable status: int,
  mutable pc: int,
}

let new = () => {
  {
    register_a: 0,
    register_x: 0,
    register_y: 0,
    stack_pointer: 0,
    status: 0,
    pc: 0,
  }
}
let update_zero_and_negative_flags = (cpu, result) => {
  cpu.status = if result == 0 {
    lor(cpu.status, 0b0000_0010)
  } else {
    land(cpu.status, 0b1111_1101)
  }
  cpu.status = if land(result, 0b1000_0000) == 0 {
    land(cpu.status, 0b0111_111)
  } else {
    lor(cpu.status, 0b1000_0000)
  }
}

let lda = (cpu, param) => {
  cpu.register_a = param
  update_zero_and_negative_flags(cpu, cpu.register_a)
}
let tax = cpu => {
  cpu.register_x = cpu.register_a
  update_zero_and_negative_flags(cpu, cpu.register_x)
}

let interpret = (cpu, program) => {
  cpu.register_a = 0
  cpu.status = 0
  cpu.pc = 0
  let break = ref(false)
  while !break.contents {
    let op = program[cpu.pc]
    cpu.pc = cpu.pc + 1
    switch op {
    | 0x00 => break := true
    | 0xA9 => {
        let param = program[cpu.pc]
        cpu.pc = cpu.pc + 1
        lda(cpu, param)
      }
    | 0xAA => tax(cpu)
    | _ => ()
    }
  }
}
