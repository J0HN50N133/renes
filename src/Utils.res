let hexdump = i => i->Js.Int.toStringWithRadix(~radix=16)
let bindump = i => i->Js.Int.toStringWithRadix(~radix=2)
type l_or_r = L | R
let padding = (~l_or_r=R, s, w, c) => {
  let lack = Js.Math.max_int(w - s->Js.String2.length, 0)
  switch l_or_r {
  | R => s ++ Js.String2.repeat(c, lack)
  | L => Js.String2.repeat(c, lack) ++ s
  }
}
let padding_with_0 = (s, w) => {
  let lack = Js.Math.max_int(w - s->Js.String2.length, 0)
  Js.String2.repeat("0", lack) ++ s
}
module Exn = {
  exception TODO(string)
}

module type MInt = {
  type t
  let width: int
}
module MakeInt = (I: MInt) => {
  type t = I.t
  let carry = lsl(1, I.width)
  let mask = lsl(1, I.width) - 1
  let neg = num => land(-num, mask)
  let add_and_carry = (a, b) => {
    let sum = a + b
    (land(sum, mask), sum / carry)
  }
  let add = (a, b) => {
    fst(add_and_carry(a, b))
  }
  let sub = (a, b) => {
    add(a, neg(b))
  }
  let lsl_and_carry = (a, b) => {
    let sa = a->lsl(b)
    (sa->land(mask), sa->land(carry)->lsr(I.width))
  }
  let lsl = (a, b) => {
    fst(lsl_and_carry(a, b))
  }
}
module I8 = MakeInt({
  let width = 8
  type t = int
})
module I16 = MakeInt({
  let width = 16
  type t = int
})
