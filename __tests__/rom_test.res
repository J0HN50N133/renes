open Jest
open Js.TypedArray2
module U8A = Uint8Array

describe("test", () => {
  open Expect
  open! Expect.Operators
  let test_rom = Rom.test_rom()
  let rom = test_rom
  test("chr_rom: ", () =>
    expect(rom.chr_rom)->toEqual(U8A.make(Array.make(Rom.chr_rom_page_size, 2)))
  )
})
