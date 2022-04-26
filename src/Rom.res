open Js.TypedArray2
module U8A = Uint8Array
type mirroring = Vertical | Horizontal | Four_Screen
let prg_rom_page_size = 0x4000
let chr_rom_page_size = 0x2000
type rom = {
  prg_rom: U8A.t,
  chr_rom: U8A.t,
  mapper: int,
  screen_mirroring: mirroring,
}
// let nes_tag = U8A.make([0x4E, 0x45, 0x53, 0x1A])
let validate_nes_tag = tag => {
  let get = U8A.unsafe_get
  tag->U8A.length >= 4 &&
  tag->get(0) === 0x4E &&
  tag->get(1) === 0x45 &&
  tag->get(2) === 0x53 &&
  tag->get(3) === 0x1A
}
let new = raw => {
  open U8A
  let get = unsafe_get
  if !(raw->validate_nes_tag) {
    Error("File is not in iNes file format.")
  } else {
    let cb1 = raw->get(7) // control byte 1
    let cb2 = raw->get(6)
    let mapper = cb1->land(0xF0)->lor(raw->get(6)->lsr(4))
    let ines_ver = cb1->lsr(2)->land(0b11)
    if ines_ver != 0 {
      Error("Nes2.0 format is not supported")
    } else {
      let four_screen = cb2->land(0b1000) != 0
      let vertical_mirroring = cb2->land(1) != 0
      let screen_mirroring = switch (four_screen, vertical_mirroring) {
      | (true, _) => Four_Screen
      | (false, true) => Vertical
      | (false, false) => Horizontal
      }
      let prg_rom_size = raw->get(4) * prg_rom_page_size
      let chr_rom_size = raw->get(5) * chr_rom_page_size
      let skip_trainer = cb2->land(0b100) != 0
      let prg_rom_start =
        16 + if skip_trainer {
          512
        } else {
          0
        }
      let chr_rom_start = prg_rom_start + prg_rom_size
      Ok({
        prg_rom: raw->slice(~start=prg_rom_start, ~end_=prg_rom_start + prg_rom_size),
        chr_rom: raw->slice(~start=chr_rom_start, ~end_=chr_rom_start + chr_rom_size),
        mapper: mapper,
        screen_mirroring: screen_mirroring,
      })
    }
  }
}

type test_rom_format = {
  header: U8A.t,
  trainer: option<U8A.t>,
  prg_rom: U8A.t,
  chr_rom: U8A.t,
}

let create_rom = rom => {
  let raw =
    ArrayBuffer.make(
      rom.header->U8A.length +
      rom.trainer->Belt.Option.mapWithDefault(0, U8A.length) +
      rom.prg_rom->U8A.length +
      rom.chr_rom->U8A.length,
    )->U8A.fromBuffer
  let cursor = ref(0)
  let copy_into_raw = arr => {
    for i in 0 to arr->U8A.length - 1 {
      raw->U8A.unsafe_set(cursor.contents, arr->U8A.unsafe_get(i))
      cursor := cursor.contents + 1
    }
  }
  copy_into_raw(rom.header)
  switch rom.trainer {
  | Some(t) => copy_into_raw(t)
  | _ => ()
  }
  copy_into_raw(rom.prg_rom)
  copy_into_raw(rom.chr_rom)
  raw
}

let test_rom = () => {
  let test_rom_data = {
    header: U8A.make([
      0x4E,
      0x45,
      0x53,
      0x1A,
      0x02,
      0x01,
      0x31,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
      00,
    ]),
    trainer: None,
    prg_rom: U8A.make(Array.make(2 * prg_rom_page_size, 1)),
    chr_rom: U8A.make(Array.make(1 * chr_rom_page_size, 2)),
  }
  let test_raw = create_rom(test_rom_data)
  let maybe_rom = new(test_raw)
  Belt.Result.getExn(maybe_rom)
}
