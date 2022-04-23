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
exception UnSupportedAddressingMode
exception ErrorInstruction
type addressing_mode =
  | Immediate
  | ZeroPage
  | ZeroPage_X
  | ZeroPage_Y
  | Absolute
  | Absolute_X
  | Absolute_Y
  | Indirect_X
  | Indirect_Y
  | NoneAddressing

type instruction = {
  bin: int,
  code: opcode,
  bytes: int,
  cycles: int,
  mode: addressing_mode,
}
let new = (bin, code, bytes, cycles, mode) => (
  bin,
  {
    bin: bin,
    code: code,
    bytes: bytes,
    cycles: cycles,
    mode: mode,
  },
)
module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})
let instruction_table = Belt.HashMap.fromArray(
  ~id=module(IntHash),
  [
    /* code | instruction name | bytes | address mode */
    new(0x00, BRK, 1, 7, NoneAddressing),
    new(0xA9, LDA, 2, 2, Immediate),
    new(0xA5, LDA, 2, 3, ZeroPage),
    new(0xB5, LDA, 2, 4, ZeroPage_X),
    new(0xAD, LDA, 3, 4, Absolute),
    new(0xBD, LDA, 3, 4, Absolute_X),
    new(0xB9, LDA, 3, 4, Absolute_Y),
    new(0xA1, LDA, 2, 6, Indirect_X),
    new(0xB1, LDA, 2, 5, Indirect_Y),
    new(0xAA, TAX, 1, 2, NoneAddressing),
    new(0xE8, INX, 1, 2, NoneAddressing),
    new(0xC8, INY, 1, 2, NoneAddressing),
    new(0x85, STA, 2, 3, ZeroPage),
    new(0x95, STA, 2, 4, ZeroPage_X),
    new(0x8D, STA, 3, 4, Absolute),
    new(0x9D, STA, 3, 5, Absolute_X),
    new(0x99, STA, 3, 5, Absolute_Y),
    new(0x81, STA, 2, 6, Indirect_X),
    new(0x91, STA, 2, 6, Indirect_Y),
  ],
)
