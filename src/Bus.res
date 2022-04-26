open Js.TypedArray2
module U8A = Uint8Array
/* memory map */
let ram = 0x0000
let ram_mirrors_end = 0x1FFF
let ram_addr_mask = 0x07FF
let prg_rom = 0x8000
let prg_rom_end = 0xFFFF
let sram = 0x6000
let sram_end = 0x7FFF
let ppu_registers = 0x2000
let ppu_registers_mirrors_end = 0x3FFF
let ppu_registers_mask = 0x2007

type bus = {cpu_vram: U8A.t, rom: Rom.rom}
let new = rom => {
  {
    cpu_vram: 2048->ArrayBuffer.make->U8A.fromBuffer,
    rom: rom,
  }
}

let read_prg_rom = (bus, addr) => {
  let mapped_addr = addr - prg_rom
  if bus.rom.prg_rom->U8A.length === 0x4000 && mapped_addr >= 0x4000 {
    bus.rom.prg_rom->U8A.unsafe_get(mod(mapped_addr, 0x4000))
  } else {
    bus.rom.prg_rom->U8A.unsafe_get(mapped_addr)
  }
}

let mem_read = (bus, addr) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end => {
      let mask_addr = land(addr, ram_addr_mask)
      bus.cpu_vram->U8A.unsafe_get(mask_addr)
    }
  | _ if ppu_registers <= addr && addr <= ppu_registers_mirrors_end => {
      let mask_addr = land(addr, ppu_registers_mask)
      raise(Utils.Exn.TODO)
    }
  | _ if prg_rom <= addr && addr <= prg_rom_end => bus->read_prg_rom(addr)
  | _ => {
      Js.log("Ignoring mem access at 0x" ++ Js.Int.toStringWithRadix(addr, ~radix=16))
      0
    }
  }
}
let mask_ram_addr = addr => land(addr, ram_addr_mask)
let mem_read_2bytes = (bus, addr) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end => {
      let lo = bus.cpu_vram->U8A.unsafe_get(mask_ram_addr(addr))
      let hi = bus.cpu_vram->U8A.unsafe_get(mask_ram_addr(addr + 1))
      lor(lsl(hi, 8), lo)
    }
  | _ if ppu_registers <= addr && addr <= ppu_registers_mirrors_end => {
      let mask_addr = land(addr, ppu_registers_mask)
      raise(Utils.Exn.TODO)
    }
  | _ => {
      Js.log("Ignoring mem access at 0x" ++ Js.Int.toStringWithRadix(addr, ~radix=16))
      0
    }
  }
}
exception InvalidWrite(string)
let mem_write = (bus, addr, data) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end =>
    bus.cpu_vram->U8A.unsafe_set(mask_ram_addr(addr), data)
  | _ if ppu_registers <= addr && addr <= ppu_registers_mirrors_end => {
      let mask_addr = land(addr, ppu_registers_mask)
      raise(Utils.Exn.TODO)
    }
  | _ if prg_rom <= addr && addr <= prg_rom_end =>
    raise(InvalidWrite("Attempt to write to Cartridge ROM space"))
  | _ => Js.log("Ignoring mem access at 0x" ++ Js.Int.toStringWithRadix(addr, ~radix=16))
  }
}
let mem_write_2bytes = (bus, addr, data) => {
  let hi = lsr(data, 8)
  let lo = land(data, 0xff)
  bus->mem_write(addr, lo)
  bus->mem_write(addr, hi)
}
