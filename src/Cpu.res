type cpu = {
  mutable register_a: int,
  mutable status: int,
  mutable pc: int,
}
let new = () => {
  {
    register_a: 0,
    status: 0,
    pc: 0,
  }
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
        cpu.register_a = param
        cpu.status = if cpu.register_a == 0 {
          lor(cpu.status, 0b0000_0010)
        } else {
          land(cpu.status, 0b1111_1101)
        }
        cpu.status = if land(cpu.register_a, 0b1000_0000) != 0 {
          lor(cpu.status, 0b1000_0000)
        } else {
          land(cpu.status, 0b0111_1111)
        }
      }
    | _ => ()
    }
  }
}
