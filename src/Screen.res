open Js.TypedArray2
module Dom = Webapi.Dom
module Canvas = Webapi.Canvas
module C2d = Webapi.Canvas.Canvas2d
module U8CA = Uint8ClampedArray
let color = byte => {
  switch byte {
  | 0 => (0, 0, 0)
  | 1 => (0xFF, 0xFF, 0xFF)
  | 2 | 9 => (187, 187, 187)
  | 3 | 10 => (0xFF, 0, 0)
  | 4 | 11 => (0, 0xFF, 0)
  | 5 | 12 => (0, 0, 0xFF)
  | 6 | 13 => (0XFF, 0, 0xFF)
  | 7 | 15 => (0xFF, 0xFF, 0)
  | _ => (0, 0xFF, 0xFF)
  }
}
let frame_read = (frame, idx) => {
  let data = frame->Webapi__Dom__Image.data
  (data->U8CA.unsafe_get(idx), data->U8CA.unsafe_get(idx + 1), data->U8CA.unsafe_get(idx + 2))
}
let frame_set = (frame, idx, (r, g, b)) => {
  let data = frame->Webapi__Dom__Image.data
  data->U8CA.unsafe_set(idx, r)
  data->U8CA.unsafe_set(idx + 1, g)
  data->U8CA.unsafe_set(idx + 2, b)
  data->U8CA.unsafe_set(idx + 3, 255)
}
let read_screen_state = (cpu: Cpu.cpu, frame) => {
  open Cpu
  let frame_idx = ref(0)
  let update = ref(false)
  for i in 0x0200 to 0x0600 - 1 {
    let color_idx = cpu->mem_read(i)
    let color = color(color_idx)
    if frame->frame_read(frame_idx.contents) !== color {
      frame->frame_set(frame_idx.contents, color)
      update := true
    }
    frame_idx := frame_idx.contents + 4
  }
  update.contents
}
let scale = 10
let draw = (canvas, frame, cpu: Cpu.cpu) => {
  if read_screen_state(cpu, frame) {
    let ctx = canvas->Canvas.CanvasElement.getContext2d
    let scaledImg =
      ctx->C2d.createImageDataCoords(
        ~width=32. *. float_of_int(scale),
        ~height=32. *. float_of_int(scale),
      )
    for row in 0 to 31 {
      for col in 0 to 31 {
        let srcIndex = (row * 32 + col) * 4
        for y in 0 to scale - 1 {
          let destRow = row * scale + y
          for x in 0 to scale - 1 {
            let destCol = col * scale + x
            let destIndex = (destRow * 32 * scale + destCol) * 4
            frame_set(scaledImg, destIndex, frame_read(frame, srcIndex))
          }
        }
      }
    }
    ctx->C2d.putImageData(~imageData=scaledImg, ~dx=20., ~dy=20.)
  }
}
