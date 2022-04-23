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
  mutable memory: array<int>,
}
exception UnSupportedAddressingMode
type addressing_mode =
  | Immediate
  | ZeroPage
  | ZeroPage_X
  | ZeroPage_Y
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect_X
  | Indirect_Y
  | NoneAddressing

let new = () => {
  {
    register_a: 0,
    register_x: 0,
    register_y: 0,
    stack_pointer: 0,
    status: 0,
    pc: 0,
    memory: Belt.Array.make(0xFFFF, 0),
  }
}
let mem_read = (cpu, addr) => cpu.memory[addr]
let mem_read_2bytes = (cpu, addr) => {
  let lo = mem_read(cpu, addr)
  let hi = mem_read(cpu, addr + 1)
  lor(lsl(hi, 8), lo)
}
let mem_write = (cpu, addr, data) => cpu.memory[addr] = data
let mem_write_2bytes = (cpu, addr, data) => {
  let hi = lsr(data, 8)
  let lo = land(data, 0xff)
  mem_write(cpu, addr, lo)
  mem_write(cpu, addr + 1, hi)
}
let update_zero_and_negative_flags = (cpu, result) => {
  cpu.status = switch result {
  | 0 => lor(cpu.status, 0b0000_0010)
  | _ => land(cpu.status, 0b1111_1101)
  }
  cpu.status = switch land(result, 0b1000_0000) {
  | 0 => land(cpu.status, 0b0111_1111)
  | _ => lor(cpu.status, 0b1000_0000)
  }
}
let reset = cpu => {
  cpu.register_a = 0
  cpu.register_x = 0
  cpu.status = 0

  cpu.pc = mem_read_2bytes(cpu, 0xFFFC)
}
let update_overflow_flag_and_prune_result = (cpu, result) => {
  cpu.status = switch result {
  | _ if result > 0xff => lor(cpu.status, 0b0100_0000)
  | _ => land(cpu.status, 0b1011_1111)
  }
  mod(result, 256)
}
let wrapping_add = (bits, a, b) => {
  mod(a + b, Js.Math.pow_float(~base=2., ~exp=bits->float_of_int)->int_of_float)
}

let wrapping_add_8 = wrapping_add(8)
let wrapping_add_16 = wrapping_add(16)
let get_operand_address = (cpu, mode) =>
  switch mode {
  | Immediate => cpu.pc
  | ZeroPage => mem_read(cpu, cpu.pc)
  | Absolute => mem_read_2bytes(cpu, cpu.pc)
  | ZeroPage_X => {
      let pos = mem_read(cpu, cpu.pc)
      let addr = wrapping_add_16(pos, cpu.register_x)
      addr
    }
  | ZeroPage_Y => {
      let pos = mem_read(cpu, cpu.pc)
      let addr = wrapping_add_16(pos, cpu.register_y)
      addr
    }
  | Absolute_X => {
      let base = mem_read_2bytes(cpu, cpu.pc)
      let addr = wrapping_add_16(base, cpu.register_x)
      addr
    }
  | Absolute_Y => {
      let base = mem_read_2bytes(cpu, cpu.pc)
      let addr = wrapping_add_16(base, cpu.register_y)
      addr
    }
  | Indirect_X => {
      let base = mem_read(cpu, cpu.pc)
      let ptr = wrapping_add_8(base, cpu.register_x)
      let lo = mem_read(cpu, ptr)
      let hi = mem_read(cpu, wrapping_add_8(ptr, 1))
      lor(lsl(hi, 8), lo)
    }
  | Indirect_Y => {
      let base = mem_read(cpu, cpu.pc)
      let lo = mem_read(cpu, base)
      let hi = mem_read(cpu, wrapping_add_8(base, 1))
      let deref_base = lor(lsl(hi, 8), lo)
      let deref = wrapping_add_16(deref_base, cpu.register_y)
      deref
    }
  | NoneAddressing => raise(UnSupportedAddressingMode)
  }
let lda = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  cpu.register_a = mem_read(cpu, addr)
  update_zero_and_negative_flags(cpu, cpu.register_a)
}
let tax = cpu => {
  cpu.register_x = cpu.register_a
  Js.log(cpu.register_x)
  update_zero_and_negative_flags(cpu, cpu.register_x)
}
let inx = cpu => {
  cpu.register_x = cpu.register_x + 1
  cpu.register_x = update_overflow_flag_and_prune_result(cpu, cpu.register_x)
  update_zero_and_negative_flags(cpu, cpu.register_x)
}
let iny = cpu => {
  cpu.register_y = cpu.register_y + 1
  cpu.register_y = update_overflow_flag_and_prune_result(cpu, cpu.register_y)
  update_zero_and_negative_flags(cpu, cpu.register_y)
}
let interpret = (cpu, program) => {
  cpu.pc = 0
  let break = ref(false)
  while !break.contents {
    let op = program[cpu.pc]
    cpu.pc = cpu.pc + 1
    switch op {
    | 0x00 => break := true
    | 0xA9 => {
        lda(cpu, Immediate)
        cpu.pc = cpu.pc + 1
      }
    | 0xA5 => {
        lda(cpu, ZeroPage)
        cpu.pc = cpu.pc + 1
      }
    | 0xB5 => {
        lda(cpu, ZeroPage_X)
        cpu.pc = cpu.pc + 1
      }
    | 0xAD => {
        lda(cpu, Absolute)
        cpu.pc = cpu.pc + 2
      }
    | 0xBD => {
        lda(cpu, Absolute_X)
        cpu.pc = cpu.pc + 2
      }
    | 0xB9 => {
        lda(cpu, Absolute_Y)
        cpu.pc = cpu.pc + 2
      }
    | 0xA1 => {
        lda(cpu, Indirect_X)
        cpu.pc = cpu.pc + 1
      }
    | 0xB1 => {
        lda(cpu, Indirect_Y)
        cpu.pc = cpu.pc + 1
      }
    | 0xAA => tax(cpu)
    | 0xE8 => inx(cpu)
    | 0xC8 => iny(cpu)
    | _ => ()
    }
  }
}
let load = (cpu, program) => {
  cpu.pc = 0x80
  for x in 0 to Belt.Array.length(program) - 1 {
    cpu.memory[0x8000 + x] = program[x]
  }
}
/*
let run = cpu => {
  let break = ref(false)
  while !break.contents {
  }
}
let load_and_run = (cpu, program) => {
  load(cpu, program)
  reset(cpu)
  run(cpu)
}
*/
