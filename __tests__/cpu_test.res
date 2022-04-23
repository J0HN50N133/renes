open Jest
open Js.TypedArray2
let array2bytes = arr => {
  let len = Belt.Array.length(arr)
  let buffer = Uint8Array.fromBuffer(ArrayBuffer.make(len))
  for i in 0 to len - 1 {
    Uint8Array.unsafe_set(buffer, i, arr[i])
  }
  buffer
}
describe("test_0xa9_lda_immidiate_load_data", () => {
  open Expect
  let cpu = Cpu.new()
  Cpu.interpret(cpu, array2bytes([0xa9, 0x05, 0x00]))
  test("register_a", () => expect(cpu.register_a)->toBe(0x05))
  test("cpu_status_flag_2", () => expect(land(cpu.status, 0b0000_0010))->toBe(0b00))
  test("cpu_status_flag_7", () => expect(land(cpu.status, 0b1000_0000))->toBe(0b00))
})

describe("test_0xa9_lda_zero_flag", () => {
  open Expect
  let cpu = Cpu.new()
  Cpu.interpret(cpu, array2bytes([0xa9, 0x00, 0x00]))
  test("cpu_status", () => expect(land(cpu.status, 0b0000_0010))->toBe(0b10))
})

describe("test_0xaa_tax_move_a_to_x", () => {
  open Expect
  let cpu = Cpu.new()
  cpu.register_a = 10
  Cpu.interpret(cpu, array2bytes([0xaa, 0x00]))
  test("register_x", () => expect(cpu.register_x)->toBe(10))
})

describe("test_5_ops_workding_together", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new()
  Cpu.interpret(cpu, array2bytes([0xa9, 0xc0, 0xaa, 0xe8, 0x00]))
  test("register_x", () => expect(cpu.register_x) === 0xc1)
})

describe("test_inx_overflow", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new()
  cpu.register_x = 0xff
  Cpu.interpret(cpu, array2bytes([0xe8, 0xe8, 0x00]))
  test("register_x", () => expect(cpu.register_x) === 0x01)
})

describe("test_iny_overflow", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new()
  cpu.register_y = 0xff
  Cpu.interpret(cpu, array2bytes([0xc8, 0xc8, 0x00]))
  test("register_x", () => expect(cpu.register_y) === 0x01)
})
describe("test_lda_from_memory", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new()
  Cpu.mem_write(cpu, 0x10, 0x55)
  Cpu.load_and_run(cpu, array2bytes([0xa5, 0x10, 0x00]))
  test("register_a", () => expect(cpu.register_a) === 0x55)
})
