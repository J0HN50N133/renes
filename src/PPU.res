open Js.TypedArray2
open Utils
module U8A = Uint8Array

module type MAddrRegister = {
  type t
  let new: unit => t
  let get: t => I16.t
  let set: (t, I16.t) => unit
  let update: (t, I8.t) => unit
  let increment: (t, I8.t) => unit
  let reset_latch: t => unit
}

module AddrRegister: MAddrRegister = {
  type t = {
    mutable hi: I8.t,
    mutable lo: I8.t,
    mutable hi_ptr: bool,
  }
  let new = () => {{hi: 0, lo: 0, hi_ptr: true}}
  let get = r => {
    lor(I16.lsl(r.hi, 8), r.lo)
  }
  let set = (r, data: I16.t) => {
    r.hi = data->lsr(8)
    r.lo = data->land(0xff)
  }
  let check = r => {
    if r->get > Bus.ppu_registers_mirrors_end {
      r->set(r->get->land(Bus.ppu_registers_mirrors_end))
    }
  }
  let update = (r, data: I8.t) => {
    if r.hi_ptr {
      r.hi = data
    } else {
      r.lo = data
    }
    check(r)
    r.hi_ptr = !r.hi_ptr
  }
  let increment = (r, inc: I8.t) => {
    let (a, c) = r.lo->I8.add_and_carry(inc)
    r.lo = a
    r.hi = r.hi->I8.add(c)
    if r->get > Bus.ppu_registers_mirrors_end {
      r->set(r->get->land(Bus.ppu_registers_mirrors_end))
    }
  }
  let reset_latch = r => r.hi_ptr = true
}
module ControlRegister = {
  type t = {mutable bits: I8.t}
  let nametable1 = 0b00000001
  let nametable2 = 0b00000010
  let vram_add_increment = 0b00000100
  let sprite_pattern_addr = 0b00001000
  let backround_pattern_addr = 0b00010000
  let sprite_size = 0b00100000
  let master_slave_select = 0B01000000
  let generate_nmi = 0b10000000
  let new = () => {bits: 0}
  let vram_addr_increment = r => {
    if land(r.bits, vram_add_increment) === 0 {
      1
    } else {
      32
    }
  }
  let update = (r, data: I8.t) => {
    r.bits = data
  }
}
module type MNesPPU = {
  type t
  let new: (U8A.t, Rom.mirroring) => t
  let write_to_ppu_addr: (t, I8.t) => unit
  let write_to_ctrl: (t, I8.t) => unit
  let incr_vram_addr: t => unit
  let read_data: t => I8.t
}
module NesPPU: MNesPPU = {
  type t = {
    chr_rom: U8A.t,
    palette_table: U8A.t,
    vram: U8A.t,
    oam_data: U8A.t,
    mirroring: Rom.mirroring,
    addr: AddrRegister.t,
    ctrl: ControlRegister.t,
    mutable internal_data_buf: I8.t,
  }

  let make_u8a = i => ArrayBuffer.make(i)->U8A.fromBuffer
  let new = (chr_rom, mirroring) => {
    {
      chr_rom: chr_rom,
      mirroring: mirroring,
      palette_table: make_u8a(32),
      vram: make_u8a(2048),
      oam_data: make_u8a(64 * 4),
      addr: AddrRegister.new(),
      ctrl: ControlRegister.new(),
      internal_data_buf: 0,
    }
  }
  let write_to_ppu_addr = (self, value) => {
    self.addr->AddrRegister.update(value)
  }
  let write_to_ctrl = (self, value) => {
    self.ctrl->ControlRegister.update(value)
  }
  let incr_vram_addr = self => {
    self.addr->AddrRegister.increment(self.ctrl->ControlRegister.vram_addr_increment)
  }
  // see https://www.nesdev.org/wiki/Mirroring
  let mirror_vram_addr = (self, addr) => {
    let mirrored_vram = addr->land(0x2FFF)
    let vram_index = mirrored_vram - 0x2000
    let name_table = vram_index / 0x400
    switch (self.mirroring, name_table) {
    | (Rom.Vertical, 2) | (Rom.Vertical, 3) | (Rom.Horizontal, 3) => vram_index - 0x800
    | (Rom.Horizontal, 2) | (Rom.Horizontal, 1) => vram_index - 0x400
    | _ => vram_index
    }
  }
  let read_data = self => {
    let addr = self.addr->AddrRegister.get
    self->incr_vram_addr
    switch addr {
    | x if 0 <= x && x <= 0x1FFF => {
        let result = self.internal_data_buf
        self.internal_data_buf = self.chr_rom->U8A.unsafe_get(addr)
        result
      }
    | x if 0x2000 <= x && x <= 0x2fff => {
        let result = self.internal_data_buf
        self.internal_data_buf = self.vram->U8A.unsafe_get(self->mirror_vram_addr(addr))
        result
      }
    | x if 0x3000 <= x && x <= 0x3eff =>
      raise(Exn.TODO("addr space 0x3000..0x3eff is not expected to be used, requested"))
    | 0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
        let add_mirror = addr - 0x10
        self.palette_table->U8A.unsafe_get(add_mirror - 0x3f00)
      }
    | x if 0x3f00 <= x && x <= 0x3fff => self.palette_table->U8A.unsafe_get(addr - 0x3f00)
    | _ => failwith("unexpected access to mirrored space " ++ addr->hexdump)
    }
  }
}
