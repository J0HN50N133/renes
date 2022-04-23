open Jest

describe("test_0xa9_lda_immidiate_load_data", () => {
  open Expect
  let cpu = Cpu.new()
  Cpu.interpret(cpu, [0xa9, 0x05, 0x00])
  test("register_a", () => expect(cpu.register_a)->toBe(0x05))
  test("cpu_status_flag_2", () => expect(land(cpu.status, 0b0000_0010))->toBe(0b00))
  test("cpu_status_flag_7", () => expect(land(cpu.status, 0b1000_0000))->toBe(0b00))
})
