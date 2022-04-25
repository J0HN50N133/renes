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
let raw_draw = %raw(`
  function(canvas, scale, frame){
    let ctx = canvas.getContext('2d')
      let imageData = ctx.createImageData(32*scale, 32*scale)
      for(let row = 0; row < 32; row++){
        for(let col = 0; col < 32; col++){

          let old_index = (row*32+col)*4
          for(let y = 0; y < scale; y++){
            let destRow = row*scale + y
            for(let x = 0; x < scale; x++){
              let destCol = col*scale + x
              let index = (destRow*32*scale+destCol)*4
              let pixel = frame[row*32+col]
              let rgb = color(pixel)
              imageData.data[index] = frame.data[old_index]
              imageData.data[index+1] = frame.data[old_index+1]
              imageData.data[index+2] = frame.data[old_index+2]
              imageData.data[index+3] = 255
            }
          }
        }
      }
    ctx.putImageData(imageData, 20, 20)
  }
`)
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
let scale = 10.
let draw = (canvas, frame, cpu: Cpu.cpu) => {
  if read_screen_state(cpu, frame) {
    let _ = raw_draw(canvas, scale, frame)
  }
}
