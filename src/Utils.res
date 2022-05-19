module Exn = {
  exception TODO
}

module type MInt = {
  let width: int
}
module MakeInt = (I: MInt) => {
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
module MI8: MInt = {
  let width = 8
}
module MI16: MInt = {
  let width = 16
}
module I8 = MakeInt(MI8)
module I16 = MakeInt(MI16)
