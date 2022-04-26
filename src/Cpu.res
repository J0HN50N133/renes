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
open Instruction
open Js.TypedArray2
type cpu = {
  mutable register_a: int,
  mutable register_x: int,
  mutable register_y: int,
  mutable stack_pointer: int,
  mutable pc: int,
  mutable stack: int,
  mutable jumped: bool, // whether pc just jump from other place
  mutable n: int,
  mutable v: int,
  mutable g: int,
  mutable b: int,
  mutable d: int,
  mutable i: int,
  mutable z: int,
  mutable c: int,
  bus: Bus.bus,
}
let stack_init_val = 0xfd
let stack_reset = cpu => cpu.stack = stack_init_val

let status_2_vector = cpu => {
  cpu.n * 128 + cpu.v * 64 + cpu.g * 32 + cpu.b * 16 + cpu.d * 8 + cpu.i * 4 + cpu.z * 2 + cpu.c
}

let vector_2_status = (cpu, vector) => {
  cpu.n = land(vector, 0b1000_0000) === 0 ? 0 : 1
  cpu.v = land(vector, 0b0100_0000) === 0 ? 0 : 1
  cpu.g = land(vector, 0b0010_0000) === 0 ? 0 : 1
  cpu.b = land(vector, 0b0001_0000) === 0 ? 0 : 1
  cpu.d = land(vector, 0b0000_1000) === 0 ? 0 : 1
  cpu.i = land(vector, 0b0000_0100) === 0 ? 0 : 1
  cpu.z = land(vector, 0b0000_0010) === 0 ? 0 : 1
  cpu.c = land(vector, 0b0000_0001) === 0 ? 0 : 1
}

let new = () => {
  {
    register_a: 0,
    register_x: 0,
    register_y: 0,
    stack_pointer: 0,
    pc: 0,
    bus: Bus.new(),
    stack: stack_init_val,
    jumped: false,
    n: 0,
    v: 0,
    g: 0,
    b: 0,
    d: 0,
    i: 0,
    z: 0,
    c: 0,
  }
}
let mem_read = (cpu, addr) => {
  open Bus
  cpu.bus->mem_read(addr)
}
let mem_read_2bytes = (cpu, addr) => {
  open Bus
  cpu.bus->mem_read_2bytes(addr)
}
/* mem_write: write [data] to the [addr] of [cpu]'s memory */
let mem_write = (cpu, addr, data) => {
  open Bus
  cpu.bus->mem_write(addr, data)
}
let mem_write_2bytes = (cpu, addr, data) => {
  open Bus
  cpu.bus->mem_write_2bytes(addr, data)
}

let stack_push = (cpu, data) => {
  mem_write(cpu, cpu.stack + cpu.stack_pointer, data)
  cpu.stack_pointer = cpu.stack_pointer - 1
}
let stack_push_2bytes = (cpu, data) => {
  let hi = lsr(data, 8)
  let lo = land(data, 0xff)
  stack_push(cpu, hi)
  stack_push(cpu, lo)
}
let stack_pop = cpu => {
  cpu.stack_pointer = cpu.stack_pointer + 1
  mem_read(cpu, cpu.stack + cpu.stack_pointer)
}
let stack_pop_2bytes = cpu => {
  let lo = stack_pop(cpu)
  let hi = stack_pop(cpu)
  lor(lsl(hi, 8), lo)
}
let update_zero_flag = (cpu, result) => {
  cpu.z = switch result {
  | 0 => 1
  | _ => 0
  }
}
let update_negative_flag = (cpu, result) => {
  cpu.n = switch land(result, 0b1000_0000) {
  | 0 => 0
  | _ => 1
  }
}
let update_zero_and_negative_flags = (cpu, result) => {
  update_zero_flag(cpu, result)
  update_negative_flag(cpu, result)
}

let update_overflow_flag_and_prune_result = (cpu, result) => {
  cpu.v = switch result {
  | _ if result > 0xff => 1
  | _ => 0
  }
  mod(result, 256)
}

let reset = cpu => {
  cpu.register_a = 0
  cpu.register_x = 0
  cpu.register_y = 0
  cpu.stack_pointer = 0
  cpu.jumped = false
  vector_2_status(cpu, 0)
  cpu.g = 1
  cpu.i = 1
  cpu.b = 1

  cpu.pc = mem_read_2bytes(cpu, 0xFFFC)
  stack_reset(cpu)
}
let wrapping_add_with_carry = (bits, a, b) => {
  let sum = a + b
  let bound = bits->float_of_int->(x => 2. ** x)->int_of_float
  (mod(sum, bound), sum / bound)
}

let wrapping_add = (bits, a, b) => {
  let (x, _) = wrapping_add_with_carry(bits, a, b)
  x
}

let wrapping_add_8 = wrapping_add(8)
let wrapping_add_with_carry_8 = wrapping_add_with_carry(8)
let wrapping_add_16 = wrapping_add(16)
let wrapping_add_with_carry_16 = wrapping_add_with_carry(16)
let get_operand_address = (cpu, mode) =>
  switch mode {
  | Immediate => cpu.pc
  | Relative => {
      let addr = mem_read(cpu, cpu.pc)
      let addr = if addr < 0x80 {
        cpu.pc + addr
      } else {
        cpu.pc + addr - 256
      }
      // -1, since the pc have been move 1 in get_instruction
      addr + 1
    }
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
  | Indirect => {
      let mem_addr = mem_read_2bytes(cpu, cpu.pc)
      if land(mem_addr, 0xFF) == 0xFF {
        // paging
        let lo = mem_read(cpu, mem_addr)
        let hi = mem_read(cpu, land(mem_addr, 0xFF00))
        lor(lsl(hi, 8), lo)
      } else {
        mem_read_2bytes(cpu, mem_addr)
      }
    }
  | NoneAddressing => raise(UnSupportedAddressingMode)
  }

let load_to_register = (cpu, value) => {
  update_zero_and_negative_flags(cpu, value)
  value
}
let add_to_register_a = (cpu, data) => {
  let sum = cpu.register_a + data + cpu.c
  cpu.c = sum > 0xff ? 1 : 0
  let result = land(sum, 0xff)
  cpu.v = if land(land(lxor(data, result), lxor(result, cpu.register_a)), 0x80) === 1 {
    1
  } else {
    0
  }
  cpu.register_a = load_to_register(cpu, result)
}
let adc = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  add_to_register_a(cpu, m)
}
let logic_and_or_op = (cpu, mode, op) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  cpu.register_a = op(cpu.register_a, m)
  update_zero_and_negative_flags(cpu, cpu.register_a)
}
let and_ = (cpu, mode) => {logic_and_or_op(cpu, mode, land)}
let ora = (cpu, mode) => {logic_and_or_op(cpu, mode, lor)}
let get_operand_value_in_mem_or_a = (cpu, mode) => {
  switch mode {
  | NoneAddressing => cpu.register_a
  | _ => {
      let addr = get_operand_address(cpu, mode)
      mem_read(cpu, addr)
    }
  }
}
let write_operand_to_mem_or_a = (cpu, mode, value) => {
  let wrap_value = land(value, 0xff)
  switch mode {
  | NoneAddressing => cpu.register_a = wrap_value
  | _ => mem_write(cpu, get_operand_address(cpu, mode), wrap_value)
  }
  update_zero_and_negative_flags(cpu, value)
}
let asl = (cpu, mode) => {
  let val = get_operand_value_in_mem_or_a(cpu, mode)
  cpu.c = land(lsr(val, 7), 1)
  write_operand_to_mem_or_a(cpu, mode, lsl(val, 1))
}
let lsr_ = (cpu, mode) => {
  let val = get_operand_value_in_mem_or_a(cpu, mode)
  cpu.c = land(val, 1)
  write_operand_to_mem_or_a(cpu, mode, lsr(val, 1))
}
// return whether the cpu have jumped
let jmp_if_1 = (value, cpu, mode) => {
  if value === 1 {
    cpu.pc = get_operand_address(cpu, mode)
    cpu.jumped = true
  }
}
let jmp_if_0 = (value, cpu, mode) => {
  if value === 0 {
    cpu.pc = get_operand_address(cpu, mode)
    cpu.jumped = true
  }
}

let bcc = (cpu, mode) => jmp_if_0(cpu.c, cpu, mode)
let bcs = (cpu, mode) => jmp_if_1(cpu.c, cpu, mode)
let beq = (cpu, mode) => jmp_if_1(cpu.z, cpu, mode)
let bmi = (cpu, mode) => jmp_if_1(cpu.n, cpu, mode)
let bne = (cpu, mode) => jmp_if_0(cpu.z, cpu, mode)
let bpl = (cpu, mode) => jmp_if_0(cpu.n, cpu, mode)
let bvc = (cpu, mode) => jmp_if_0(cpu.v, cpu, mode)
let bvs = (cpu, mode) => jmp_if_1(cpu.v, cpu, mode)
let clc = cpu => cpu.c = 0
let cld = cpu => cpu.d = 0
let cli = cpu => cpu.i = 0
let clv = cpu => cpu.v = 0
let compare = (cpu, mode, compare_with) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  cpu.c = if m <= compare_with {
    1
  } else {
    0
  }
  update_zero_and_negative_flags(cpu, compare_with - m)
}
let cmp = (cpu, mode) => compare(cpu, mode, cpu.register_a)
let cpx = (cpu, mode) => compare(cpu, mode, cpu.register_x)
let cpy = (cpu, mode) => compare(cpu, mode, cpu.register_y)
let decrease = (cpu, value) => {
  let temp = land(value - 1, 0xff)
  update_zero_and_negative_flags(cpu, temp)
  temp
}
let dec = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  mem_write(cpu, addr, decrease(cpu, m))
}
let dex = cpu => {
  cpu.register_x = decrease(cpu, cpu.register_x)
}
let dey = cpu => {
  cpu.register_y = decrease(cpu, cpu.register_y)
}
let eor = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  cpu.register_a = lxor(m, cpu.register_a)
  update_zero_and_negative_flags(cpu, cpu.register_a)
}
let increase = (cpu, value) => {
  let temp = land(value + 1, 0xff)
  update_zero_and_negative_flags(cpu, temp)
  temp
}
let inc = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  mem_write(cpu, addr, increase(cpu, m))
}
let inx = cpu => cpu.register_x = increase(cpu, cpu.register_x)
let iny = cpu => cpu.register_y = increase(cpu, cpu.register_y)
let jmp = (cpu, mode) => {
  cpu.pc = get_operand_address(cpu, mode)
  cpu.jumped = true
}
let jsr = (cpu, mode) => {
  cpu->stack_push_2bytes(cpu.pc + 1)
  cpu.pc = get_operand_address(cpu, mode)
  cpu.jumped = true
}
let bit = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  let temp = land(m, cpu.register_a)
  cpu.z = temp === 0 ? 1 : 0
  cpu.n = land(lsl(m, 7), 1)
  cpu.v = land(lsl(m, 6), 1)
}
let lda = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  cpu.register_a = load_to_register(cpu, mem_read(cpu, addr))
}
let ldx = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  cpu.register_x = load_to_register(cpu, mem_read(cpu, addr))
}
let ldy = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  cpu.register_y = load_to_register(cpu, mem_read(cpu, addr))
}
let tax = cpu => {cpu.register_x = load_to_register(cpu, cpu.register_a)}
let tay = cpu => {cpu.register_y = load_to_register(cpu, cpu.register_a)}
let tya = cpu => {cpu.register_a = load_to_register(cpu, cpu.register_y)}
let txa = cpu => {cpu.register_a = load_to_register(cpu, cpu.register_x)}
let txs = cpu => {cpu.stack_pointer = load_to_register(cpu, cpu.register_x)}
let tsx = cpu => {cpu.register_x = load_to_register(cpu, cpu.stack_pointer)}

let store = (cpu, mode, value) => {
  let addr = get_operand_address(cpu, mode)
  mem_write(cpu, addr, value)
}
let sta = (cpu, mode) => store(cpu, mode, cpu.register_a)
let stx = (cpu, mode) => store(cpu, mode, cpu.register_x)
let sty = (cpu, mode) => store(cpu, mode, cpu.register_y)
let interpret = (cpu, program) => {
  cpu.pc = 0
  let break = ref(false)
  while !break.contents {
    let op = mem_read(cpu, cpu.pc)
    cpu.pc = cpu.pc + 1
    let instruction = Belt.HashMap.get(instruction_table, op)
    switch instruction {
    | None => raise(ErrorInstruction)
    | Some(i) =>
      switch i.code {
      | BRK => break := true
      | LDA =>
        lda(cpu, i.mode)
        cpu.pc = cpu.pc + i.bytes - 1
      | TAX => tax(cpu)
      | INX => inx(cpu)
      | INY => iny(cpu)
      | STA =>
        sta(cpu, i.mode)
        cpu.pc = cpu.pc + i.bytes - 1
      | _ => ()
      }
    }
  }
}
let load_to = (cpu, addr, program) => {
  cpu.pc = addr
  let len = Uint8Array.length(program)
  for i in 0 to len - 1 {
    cpu->mem_write(addr + i, Uint8Array.unsafe_get(program, i))
  }
  mem_write_2bytes(cpu, 0xFFFC, addr)
}
let load = (cpu, program) => {cpu->load_to(0x0600, program)}

let pc_safe = cpu => cpu.pc <= Bus.ram_mirrors_end
let php = cpu => {
  let vector = status_2_vector(cpu)
  cpu.g = 1
  cpu.b = 1
  stack_push(cpu, vector)
}
let pla = cpu => {
  let data = stack_pop(cpu)
  cpu.register_a = load_to_register(cpu, data)
}
let plp = cpu => {
  let g = cpu.g
  let vector = stack_pop(cpu)
  vector_2_status(cpu, vector)
  cpu.b = 0
  cpu.g = g
}
// A - M - ~C == A - M + C - 1
let sbc = (cpu, mode) => {
  let addr = get_operand_address(cpu, mode)
  let m = mem_read(cpu, addr)
  add_to_register_a(cpu, -m - 1)
}
let rol = (cpu, mode) => {
  let m = ref(get_operand_value_in_mem_or_a(cpu, mode))
  let old_c = cpu.c
  cpu.c = if lsr(m.contents, 7) === 0 {
    0
  } else {
    1
  }
  m := lsl(m.contents, 1)
  m := lor(m.contents, old_c)
  write_operand_to_mem_or_a(cpu, mode, m.contents)
}
let ror = (cpu, mode) => {
  let m = ref(get_operand_value_in_mem_or_a(cpu, mode))
  let old_c = cpu.c
  cpu.c = if land(m.contents, 1) === 0 {
    0
  } else {
    1
  }
  m := lsr(m.contents, 1)
  m := lor(m.contents, lsl(old_c, 7))
  write_operand_to_mem_or_a(cpu, mode, m.contents)
}
let rti = cpu => {
  let vector = stack_pop(cpu)
  vector_2_status(cpu, vector)
  cpu.b = 0
  cpu.g = 1
  cpu.pc = stack_pop_2bytes(cpu)
  cpu.jumped = true
}
let rts = cpu => {
  cpu.pc = stack_pop_2bytes(cpu) + 1
  cpu.jumped = true
}
let brk = cpu => {
  cpu.i = 1
  cpu->stack_push(cpu.pc + 1)
  cpu->stack_push(cpu->status_2_vector)
}
let step = (cpu, callback_list, break) => {
  Array.iter(f => f(cpu), callback_list)
  let op = mem_read(cpu, cpu.pc)
  cpu.pc = cpu.pc + 1
  cpu.jumped = false
  let instruction = Belt.HashMap.get(instruction_table, op)
  switch instruction {
  | None => raise(ErrorInstruction)
  | Some(i) =>
    switch i.code {
    | ADC => adc(cpu, i.mode)
    | AND => and_(cpu, i.mode)
    | ASL => asl(cpu, i.mode)
    | BCC => bcc(cpu, i.mode)
    | BCS => bcs(cpu, i.mode)
    | BEQ => beq(cpu, i.mode)
    | BMI => bmi(cpu, i.mode)
    | BNE => bne(cpu, i.mode)
    | BPL => bpl(cpu, i.mode)
    | BVC => bvc(cpu, i.mode)
    | BVS => bvs(cpu, i.mode)
    | CLC => clc(cpu)
    | CLD => cld(cpu)
    | CLI => cli(cpu)
    | CLV => clv(cpu)
    | CMP => cmp(cpu, i.mode)
    | CPX => cpx(cpu, i.mode)
    | CPY => cpy(cpu, i.mode)
    | DEC => dec(cpu, i.mode)
    | DEX => dex(cpu)
    | DEY => dey(cpu)
    | EOR => eor(cpu, i.mode)
    | INC => inc(cpu, i.mode)
    | INX => inx(cpu)
    | INY => iny(cpu)
    | JMP => jmp(cpu, i.mode)
    | JSR => jsr(cpu, i.mode)
    | BIT => bit(cpu, i.mode)
    | BRK => {
        break := true
        brk(cpu)
      }
    | LDA => lda(cpu, i.mode)
    | LDX => ldx(cpu, i.mode)
    | LDY => ldy(cpu, i.mode)
    | TAX => tax(cpu)
    | TAY => tay(cpu)
    | TYA => tya(cpu)
    | TXA => txa(cpu)
    | TXS => txs(cpu)
    | TSX => tsx(cpu)
    | STA => sta(cpu, i.mode)
    | STX => stx(cpu, i.mode)
    | STY => sty(cpu, i.mode)
    | LSR => lsr_(cpu, i.mode)
    | ORA => ora(cpu, i.mode)
    | PHA => stack_push(cpu, cpu.register_a)
    | PHP => php(cpu)
    | PLA => pla(cpu)
    | PLP => plp(cpu)
    | ROL => rol(cpu, i.mode)
    | ROR => ror(cpu, i.mode)
    | SEC => cpu.c = 1
    | SED => cpu.d = 1
    | SEI => cpu.i = 1
    | SBC => sbc(cpu, i.mode)
    | NOP => ()
    | RTI => rti(cpu)
    | RTS => rts(cpu)
    }
    switch cpu.jumped {
    | false => cpu.pc = cpu.pc + i.bytes - 1
    | _ => ()
    }
  }
}

let run_with_callback = (cpu, callback_list) => {
  let break = ref(false)
  while !break.contents && pc_safe(cpu) {
    cpu->step(callback_list, break)
  }
}
let run = run_with_callback(_, [])
let load_and_run = (cpu, program) => {
  load(cpu, program)
  reset(cpu)
  run(cpu)
}
