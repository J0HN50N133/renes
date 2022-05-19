type opcode =
  | ADC
  | AND
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | CLD
  | CLI
  | CLV
  | CMP
  | CPX
  | CPY
  | DEC
  | DEX
  | DEY
  | EOR
  | INC
  | INX
  | INY
  | JMP
  | JSR
  | LDA
  | LDX
  | LDY
  | LSR
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | ROL
  | ROR
  | RTI
  | RTS
  | SBC
  | SEC
  | SED
  | SEI
  | STA
  | STX
  | STY
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TYA
  | LAX
  | SAX
  | DCP
  | ISB
  | SLO
  | RLA
  | SRE
exception UnSupportedAddressingMode
exception ErrorInstruction
type addressing_mode =
  | Immediate
  | Relative
  | ZeroPage
  | ZeroPage_X
  | ZeroPage_Y
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect
  | Indirect_X
  | Indirect_Y
  | NoneAddressing

let opcode_of_string = str =>
  switch str {
  | "ADC" => ADC
  | "AND" => AND
  | "ASL" => ASL
  | "BCC" => BCC
  | "BCS" => BCS
  | "BEQ" => BEQ
  | "BIT" => BIT
  | "BMI" => BMI
  | "BNE" => BNE
  | "BPL" => BPL
  | "BRK" => BRK
  | "BVC" => BVC
  | "BVS" => BVS
  | "CLC" => CLC
  | "CLD" => CLD
  | "CLI" => CLI
  | "CLV" => CLV
  | "CMP" => CMP
  | "CPX" => CPX
  | "CPY" => CPY
  | "DEC" => DEC
  | "DEX" => DEX
  | "DEY" => DEY
  | "EOR" => EOR
  | "INC" => INC
  | "INX" => INX
  | "INY" => INY
  | "JMP" => JMP
  | "JSR" => JSR
  | "LDA" => LDA
  | "LDX" => LDX
  | "LDY" => LDY
  | "LSR" => LSR
  | "NOP" | "*NOP" => NOP
  | "ORA" => ORA
  | "PHA" => PHA
  | "PHP" => PHP
  | "PLA" => PLA
  | "PLP" => PLP
  | "ROL" => ROL
  | "ROR" => ROR
  | "RTI" => RTI
  | "RTS" => RTS
  | "SBC" | "*SBC" => SBC
  | "SEC" => SEC
  | "SED" => SED
  | "SEI" => SEI
  | "STA" => STA
  | "STX" => STX
  | "STY" => STY
  | "TAX" => TAX
  | "TAY" => TAY
  | "TSX" => TSX
  | "TXA" => TXA
  | "TXS" => TXS
  | "TYA" => TYA
  | "*LAX" => LAX
  | "*SAX" => SAX
  | "*DCP" => DCP
  | "*ISB" => ISB
  | "*SLO" => SLO
  | "*RLA" => RLA
  | "*SRE" => SRE
  | _ => failwith("Unknown OpCode")
  }
type instruction = {
  bin: int, // Binary Code of This OpCode
  code: opcode, // Variant of OpCode
  len: int, // size of OpCode
  cycles: int, // Cycle it takes to do op
  mode: addressing_mode, // how does this op address
  str: string, // string respresentation, only for debug
}
let new = (bin, code_str, cycles, mode) => {
  let len = switch mode {
  | NoneAddressing => 1
  | Immediate
  | Relative
  | ZeroPage
  | ZeroPage_X
  | ZeroPage_Y
  | Indirect_X
  | Indirect_Y => 2
  | Absolute | Absolute_X | Absolute_Y | Indirect => 3
  }
  (
    bin,
    {
      bin: bin,
      code: opcode_of_string(code_str),
      len: len,
      cycles: cycles,
      mode: mode,
      str: code_str,
    },
  )
}
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})
let instruction_table = Belt.HashMap.fromArray(
  ~id=module(IntHash),
  [
    /* code | instruction name | address mode */
    new(0x00, "BRK", 7, NoneAddressing),
    new(0xE8, "INX", 2, NoneAddressing),
    new(0xC8, "INY", 2, NoneAddressing),
    new(0x69, "ADC", 2, Immediate),
    new(0x65, "ADC", 3, ZeroPage),
    new(0x75, "ADC", 4, ZeroPage_X),
    new(0x6D, "ADC", 4, Absolute),
    new(0x7D, "ADC", 4, Absolute_X),
    new(0x79, "ADC", 4, Absolute_Y),
    new(0x61, "ADC", 6, Indirect_X),
    new(0x71, "ADC", 5, Indirect_Y),
    new(0x29, "AND", 2, Immediate),
    new(0x25, "AND", 3, ZeroPage),
    new(0x35, "AND", 4, ZeroPage_X),
    new(0x2D, "AND", 4, Absolute),
    new(0x3D, "AND", 4, Absolute_X),
    new(0x39, "AND", 4, Absolute_Y),
    new(0x21, "AND", 6, Indirect_X),
    new(0x31, "AND", 5, Indirect_Y),
    new(0x0A, "ASL", 2, NoneAddressing),
    new(0x06, "ASL", 5, ZeroPage),
    new(0x16, "ASL", 6, ZeroPage_X),
    new(0x0E, "ASL", 6, Absolute),
    new(0x1E, "ASL", 7, Absolute_X),
    new(0x90, "BCC", 2, Relative),
    new(0xB0, "BCS", 2, Relative),
    new(0xF0, "BEQ", 2, Relative),
    new(0x24, "BIT", 3, ZeroPage),
    new(0x2C, "BIT", 4, Absolute),
    new(0x30, "BMI", 2, Relative),
    new(0xD0, "BNE", 2, Relative),
    new(0x10, "BPL", 2, Relative),
    new(0x50, "BVC", 2, Relative),
    new(0x70, "BVS", 2, Relative),
    new(0x18, "CLC", 2, NoneAddressing),
    new(0xD8, "CLD", 2, NoneAddressing),
    new(0x58, "CLI", 2, NoneAddressing),
    new(0xB8, "CLV", 2, NoneAddressing),
    new(0xC9, "CMP", 2, Immediate),
    new(0xC5, "CMP", 3, ZeroPage),
    new(0xD5, "CMP", 4, ZeroPage_X),
    new(0xCD, "CMP", 4, Absolute),
    new(0xDD, "CMP", 4, Absolute_X),
    new(0xD9, "CMP", 4, Absolute_Y),
    new(0xC1, "CMP", 6, Indirect_X),
    new(0xD1, "CMP", 5, Indirect_Y),
    new(0xE0, "CPX", 2, Immediate),
    new(0xE4, "CPX", 3, ZeroPage),
    new(0xEC, "CPX", 4, Absolute),
    new(0xC0, "CPY", 2, Immediate),
    new(0xC4, "CPY", 3, ZeroPage),
    new(0xCC, "CPY", 4, Absolute),
    new(0xC6, "DEC", 5, ZeroPage),
    new(0xD6, "DEC", 6, ZeroPage_X),
    new(0xCE, "DEC", 6, Absolute),
    new(0xDE, "DEC", 7, Absolute_X),
    new(0xCA, "DEX", 2, NoneAddressing),
    new(0x88, "DEY", 2, NoneAddressing),
    new(0x49, "EOR", 2, Immediate),
    new(0x45, "EOR", 3, ZeroPage),
    new(0x55, "EOR", 4, ZeroPage_X),
    new(0x4D, "EOR", 4, Absolute),
    new(0x5D, "EOR", 4, Absolute_X),
    new(0x59, "EOR", 4, Absolute_Y),
    new(0x41, "EOR", 6, Indirect_X),
    new(0x51, "EOR", 5, Indirect_Y),
    new(0xE6, "INC", 5, ZeroPage),
    new(0xF6, "INC", 6, ZeroPage_X),
    new(0xEE, "INC", 6, Absolute),
    new(0xFE, "INC", 7, Absolute_X),
    new(0xE8, "INX", 2, NoneAddressing),
    new(0xC8, "INY", 2, NoneAddressing),
    new(0x4C, "JMP", 3, Absolute),
    new(0x6C, "JMP", 5, Indirect),
    new(0x20, "JSR", 6, Absolute),
    new(0xA9, "LDA", 2, Immediate),
    new(0xA5, "LDA", 3, ZeroPage),
    new(0xB5, "LDA", 4, ZeroPage_X),
    new(0xAD, "LDA", 4, Absolute),
    new(0xBD, "LDA", 4, Absolute_X),
    new(0xB9, "LDA", 4, Absolute_Y),
    new(0xA1, "LDA", 6, Indirect_X),
    new(0xB1, "LDA", 5, Indirect_Y),
    new(0xA2, "LDX", 2, Immediate),
    new(0xA6, "LDX", 3, ZeroPage),
    new(0xB6, "LDX", 4, ZeroPage_Y),
    new(0xAE, "LDX", 4, Absolute),
    new(0xBE, "LDX", 4, Absolute_Y),
    new(0xA0, "LDY", 2, Immediate),
    new(0xA4, "LDY", 3, ZeroPage),
    new(0xB4, "LDY", 4, ZeroPage_X),
    new(0xAC, "LDY", 4, Absolute),
    new(0xBC, "LDY", 4, Absolute_X),
    new(0x4A, "LSR", 2, NoneAddressing),
    new(0x46, "LSR", 5, ZeroPage),
    new(0x56, "LSR", 6, ZeroPage_X),
    new(0x4E, "LSR", 6, Absolute),
    new(0x5E, "LSR", 7, Absolute_X),
    new(0xEA, "NOP", 2, NoneAddressing),
    new(0x09, "ORA", 2, Immediate),
    new(0x05, "ORA", 3, ZeroPage),
    new(0x15, "ORA", 4, ZeroPage_X),
    new(0x0D, "ORA", 4, Absolute),
    new(0x1D, "ORA", 4, Absolute_X),
    new(0x19, "ORA", 4, Absolute_Y),
    new(0x01, "ORA", 6, Indirect_X),
    new(0x11, "ORA", 5, Indirect_Y),
    new(0x48, "PHA", 3, NoneAddressing),
    new(0x08, "PHP", 3, NoneAddressing),
    new(0x68, "PLA", 4, NoneAddressing),
    new(0x28, "PLP", 2, NoneAddressing),
    new(0x2A, "ROL", 5, NoneAddressing),
    new(0x26, "ROL", 6, ZeroPage),
    new(0x36, "ROL", 6, ZeroPage_X),
    new(0x2E, "ROL", 7, Absolute),
    new(0x3E, "ROL", 7, Absolute_X),
    new(0x6A, "ROR", 2, NoneAddressing),
    new(0x66, "ROR", 5, ZeroPage),
    new(0x76, "ROR", 6, ZeroPage_X),
    new(0x6E, "ROR", 6, Absolute),
    new(0x7E, "ROR", 7, Absolute_X),
    new(0x40, "RTI", 6, NoneAddressing),
    new(0x60, "RTS", 6, NoneAddressing),
    new(0xE9, "SBC", 2, Immediate),
    new(0xE5, "SBC", 3, ZeroPage),
    new(0xF5, "SBC", 4, ZeroPage_X),
    new(0xED, "SBC", 4, Absolute),
    new(0xFD, "SBC", 4, Absolute_X),
    new(0xF9, "SBC", 4, Absolute_Y),
    new(0xE1, "SBC", 6, Indirect_X),
    new(0xF1, "SBC", 5, Indirect_Y),
    new(0x38, "SEC", 2, NoneAddressing),
    new(0xF8, "SED", 2, NoneAddressing),
    new(0x78, "SEI", 2, NoneAddressing),
    new(0x85, "STA", 3, ZeroPage),
    new(0x95, "STA", 4, ZeroPage_X),
    new(0x8D, "STA", 4, Absolute),
    new(0x9D, "STA", 5, Absolute_X),
    new(0x99, "STA", 5, Absolute_Y),
    new(0x81, "STA", 6, Indirect_X),
    new(0x91, "STA", 6, Indirect_Y),
    new(0x86, "STX", 3, ZeroPage),
    new(0x96, "STX", 4, ZeroPage_Y),
    new(0x8E, "STX", 4, Absolute),
    new(0x84, "STY", 3, ZeroPage),
    new(0x94, "STY", 4, ZeroPage_X),
    new(0x8C, "STY", 4, Absolute),
    new(0xAA, "TAX", 2, NoneAddressing),
    new(0xA8, "TAY", 2, NoneAddressing),
    new(0xBA, "TSX", 2, NoneAddressing),
    new(0x8A, "TXA", 2, NoneAddressing),
    new(0x9A, "TXS", 2, NoneAddressing),
    new(0x98, "TYA", 2, NoneAddressing),
    new(0x04, "*NOP", 3, ZeroPage),
    new(0x14, "*NOP", 4, ZeroPage_X),
    new(0x34, "*NOP", 4, ZeroPage_X),
    new(0x44, "*NOP", 3, ZeroPage),
    new(0x54, "*NOP", 4, ZeroPage_X),
    new(0x64, "*NOP", 3, ZeroPage),
    new(0x74, "*NOP", 4, ZeroPage_X),
    new(0x0C, "*NOP", 4, Absolute),
    new(0x1C, "*NOP", 4, Absolute_X),
    new(0x3C, "*NOP", 4, Absolute_X),
    new(0x5C, "*NOP", 4, Absolute_X),
    new(0x7C, "*NOP", 4, Absolute_X),
    new(0xDC, "*NOP", 4, Absolute_X),
    new(0xFC, "*NOP", 4, Absolute_X),
    new(0xD4, "*NOP", 4, ZeroPage_X),
    new(0xF4, "*NOP", 4, ZeroPage_X),
    new(0xE2, "*NOP", 2, Immediate),
    new(0x1A, "*NOP", 2, NoneAddressing),
    new(0x3A, "*NOP", 2, NoneAddressing),
    new(0x5A, "*NOP", 2, NoneAddressing),
    new(0x7A, "*NOP", 2, NoneAddressing),
    new(0xDA, "*NOP", 2, NoneAddressing),
    new(0xFA, "*NOP", 2, NoneAddressing),
    new(0x80, "*NOP", 2, Immediate),
    new(0x82, "*NOP", 2, Immediate),
    new(0x89, "*NOP", 2, Immediate),
    new(0xC2, "*NOP", 2, Immediate),
    new(0xE2, "*NOP", 2, Immediate),
    new(0xF4, "*NOP", 4, ZeroPage_X),
    new(0xA7, "*LAX", 3, ZeroPage),
    new(0xB7, "*LAX", 4, ZeroPage_Y),
    new(0xAF, "*LAX", 4, Absolute),
    new(0xBF, "*LAX", 4, Absolute_Y),
    new(0xA3, "*LAX", 6, Indirect_X),
    new(0xB3, "*LAX", 5, Indirect_Y),
    new(0x87, "*SAX", 3, ZeroPage),
    new(0x97, "*SAX", 4, ZeroPage_Y),
    new(0x83, "*SAX", 6, Indirect_X),
    new(0x8F, "*SAX", 4, Absolute),
    new(0xEB, "*SBC", 2, Immediate),
    new(0xC7, "*DCP", 5, ZeroPage),
    new(0xD7, "*DCP", 6, ZeroPage_X),
    new(0xCF, "*DCP", 6, Absolute),
    new(0xDF, "*DCP", 7, Absolute_X),
    new(0xDB, "*DCP", 7, Absolute_Y),
    new(0xC3, "*DCP", 8, Indirect_X),
    new(0xD3, "*DCP", 8, Indirect_Y),
    new(0xE7, "*ISB", 5, ZeroPage),
    new(0xF7, "*ISB", 6, ZeroPage_X),
    new(0xEF, "*ISB", 6, Absolute),
    new(0xFF, "*ISB", 7, Absolute_X),
    new(0xFB, "*ISB", 7, Absolute_Y),
    new(0xE3, "*ISB", 8, Indirect_X),
    new(0xF3, "*ISB", 8, Indirect_Y),
    new(0x07, "*SLO", 5, ZeroPage),
    new(0x17, "*SLO", 6, ZeroPage_X),
    new(0x0F, "*SLO", 6, Absolute),
    new(0x1F, "*SLO", 7, Absolute_X),
    new(0x1B, "*SLO", 7, Absolute_Y),
    new(0x03, "*SLO", 8, Indirect_X),
    new(0x13, "*SLO", 8, Indirect_Y),
    new(0x27, "*RLA", 5, ZeroPage),
    new(0x37, "*RLA", 6, ZeroPage_X),
    new(0x2F, "*RLA", 6, Absolute),
    new(0x3F, "*RLA", 7, Absolute_X),
    new(0x3B, "*RLA", 7, Absolute_Y),
    new(0x23, "*RLA", 8, Indirect_X),
    new(0x33, "*RLA", 8, Indirect_Y),
    new(0x47, "*SRE", 5, ZeroPage),
    new(0x57, "*SRE", 6, ZeroPage_X),
    new(0x4F, "*SRE", 6, Absolute),
    new(0x5F, "*SRE", 7, Absolute_X),
    new(0x5B, "*SRE", 7, Absolute_Y),
    new(0x43, "*SRE", 8, Indirect_X),
    new(0x53, "*SRE", 8, Indirect_Y),
  ],
)
let string_of_opcode = instruction => instruction.str
