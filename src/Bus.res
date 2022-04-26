open Js.TypedArray2
let ram = 0x0000
let ram_mirrors_end = 0x1FFF
let ram_addr_mask = 0x07FF
let ppu_registers = 0x2000
let ppu_registers_mirrors_end = 0x3FFF
let ppu_registers_mask = 0x2007

type bus = {cpu_vram: Uint8Array.t}

let new = () => {
  {cpu_vram: 2048->ArrayBuffer.make->Uint8Array.fromBuffer}
}

let mem_read = (bus, addr) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end => {
      let mask_addr = land(addr, ram_addr_mask)
      bus.cpu_vram->Uint8Array.unsafe_get(mask_addr)
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
let mask_ram_addr = addr => land(addr, ram_addr_mask)
let mem_read_2bytes = (bus, addr) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end => {
      let lo = bus.cpu_vram->Uint8Array.unsafe_get(mask_ram_addr(addr))
      let hi = bus.cpu_vram->Uint8Array.unsafe_get(mask_ram_addr(addr + 1))
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

let mem_write = (bus, addr, data) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end =>
    bus.cpu_vram->Uint8Array.unsafe_set(mask_ram_addr(addr), data)
  | _ if ppu_registers <= addr && addr <= ppu_registers_mirrors_end => {
      let mask_addr = land(addr, ppu_registers_mask)
      raise(Utils.Exn.TODO)
    }
  | _ => Js.log("Ignoring mem access at 0x" ++ Js.Int.toStringWithRadix(addr, ~radix=16))
  }
}
let mem_write_2bytes = (bus, addr, data) => {
  switch addr {
  | _ if ram <= addr && addr <= ram_mirrors_end => {
      let hi = lsr(data, 8)
      let lo = land(data, 0xff)
      bus->mem_write(addr, lo)
      bus->mem_write(addr, hi)
    }
  | _ if ppu_registers <= addr && addr <= ppu_registers_mirrors_end => {
      let mask_addr = land(addr, ppu_registers_mask)
      raise(Utils.Exn.TODO)
    }
  | _ => Js.log("Ignoring mem access at 0x" ++ Js.Int.toStringWithRadix(addr, ~radix=16))
  }
}
