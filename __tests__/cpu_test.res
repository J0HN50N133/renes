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
let test_rom = Rom.test_rom()
let test_bus = Bus.new(test_rom)
describe("test_0xa9_lda_immidiate_load_data", () => {
  open Expect
  let cpu = Cpu.new(test_bus)
  Cpu.interpret(cpu, array2bytes([0xa9, 0x05, 0x00]))
  test("register_a", () => expect(cpu.register_a)->toBe(0x05))
  test("cpu_status_flag_zero", () => expect(cpu.z)->toBe(0))
  test("cpu_status_flag_negative", () => expect(cpu.n)->toBe(0))
})
describe("test_0xa9_lda_zero_flag", () => {
  open Expect
  let cpu = Cpu.new(test_bus)
  Cpu.interpret(cpu, array2bytes([0xa9, 0x00, 0x00]))
  test("cpu_status_flag_zero", () => expect(cpu.z)->toBe(1))
})

describe("test_0xaa_tax_move_a_to_x", () => {
  open Expect
  let cpu = Cpu.new(test_bus)
  cpu.register_a = 10
  Cpu.interpret(cpu, array2bytes([0xaa, 0x00]))
  test("register_x", () => expect(cpu.register_x)->toBe(10))
})

describe("test_5_ops_workding_together", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  Cpu.interpret(cpu, array2bytes([0xa9, 0xc0, 0xaa, 0xe8, 0x00]))
  test("register_x", () => expect(cpu.register_x) === 0xc1)
})

describe("test_inx_overflow", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  cpu.register_x = 0xff
  Cpu.interpret(cpu, array2bytes([0xe8, 0xe8, 0x00]))
  test("register_x", () => expect(cpu.register_x) === 0x01)
})

describe("test_iny_overflow", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  cpu.register_y = 0xff
  Cpu.interpret(cpu, array2bytes([0xc8, 0xc8, 0x00]))
  test("register_x", () => expect(cpu.register_y) === 0x01)
})
describe("test_lda_from_memory", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  Cpu.mem_write(cpu, 0x10, 0x55)
  Cpu.load_and_run(cpu, array2bytes([0xa5, 0x10, 0x00]))
  test("register_a", () => expect(cpu.register_a) === 0x55)
})

describe("lda_and_ldx", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  Cpu.mem_write(cpu, 0xfe, 0xff)
  Cpu.load_and_run(cpu, array2bytes([0xa5, 0xfe, 0xa2, 0x0c]))
  test("register_a", () => expect(cpu.register_a) === 0xff)
  test("register_x", () => expect(cpu.register_x) === 0x0c)
})

describe("asl", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  Cpu.mem_write(cpu, 0xBB, 0x80)
  Cpu.load_and_run(cpu, array2bytes([0xA5, 0xBB, 0x0A, 0x00]))
  test("register_a", () => expect(cpu.register_a) === 0x00)
  test("flag_n", () => expect(cpu.n) === 0)
  test("flag_z", () => expect(cpu.z) === 0)
  test("flag_c", () => expect(cpu.c) === 1)
})
describe("simple_draw", () => {
  open Expect
  open! Expect.Operators
  let cpu = Cpu.new(test_bus)
  Cpu.load_and_run(
    cpu,
    array2bytes([
      0xa9,
      0x01,
      0x8d,
      0x00,
      0x02,
      0xa9,
      0x05,
      0x8d,
      0x01,
      0x02,
      0xa9,
      0x08,
      0x8d,
      0x02,
      0x02,
    ]),
  )
  test("pc", () => expect(cpu.pc) === 0x0610)
  test("register_a", () => expect(cpu.register_a) === 0x08)
  test("0x0200", () => expect(Cpu.mem_read(cpu, 0x0200)) === 0x01)
  test("0x0201", () => expect(Cpu.mem_read(cpu, 0x0201)) === 0x05)
  test("0x0202", () => expect(Cpu.mem_read(cpu, 0x0202)) === 0x08)
})
