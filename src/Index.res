open Webapi.Dom
open Js.TypedArray2
module Evt = Webapi.Dom.Event
module Canvas = Webapi.Canvas
module CElem = Webapi.Canvas.CanvasElement
module C2d = Webapi.Canvas.Canvas2d
let array2bytes = arr => {
  let len = Belt.Array.length(arr)
  let buffer = Uint8Array.fromBuffer(ArrayBuffer.make(len))
  for i in 0 to len - 1 {
    Uint8Array.unsafe_set(buffer, i, arr[i])
  }
  buffer
}
let unwrapUnsafely = x =>
  switch x {
  | Some(v) => v
  | None => raise(Invalid_argument("Passed `None` to unwrapUnsafely"))
  }
let load_rom_file = fileObj => {
  ()
}
@val
external setInterval: (unit => unit, int) => unit = "setInterval"
@get external getFiles: Dom.eventTarget => Webapi.FileList.t = "files"
let main = changeEvent => {
  let fileObj =
    changeEvent
    ->Event.currentTarget
    ->getFiles
    ->Webapi.FileList.item(0)
    ->Belt.Option.getExn
    ->Webapi.File.arrayBuffer
  let _ = fileObj->Js.Promise.then_(arr => {
    let raw = Rom.new(arr->Uint8Array.fromBuffer)
    switch raw {
    | Ok(rom) => {
        let cpu = Cpu.new(Bus.new(rom))
        let canvasEl = document->Document.getElementById("canvas")->unwrapUnsafely
        let frame =
          canvasEl
          ->Canvas.CanvasElement.getContext2d
          ->C2d.createImageDataCoords(~width=32., ~height=32.)
        Cpu.reset(cpu)
        let break = ref(false)
        let _ = setInterval(() => {
          Cpu.step(
            cpu,
            [
              Screen.draw(canvasEl, frame),
              cpu => Cpu.mem_write(cpu, 0xfe, Js.Math.random_int(1, 16)),
              Debug.debug,
            ],
            break,
          )
        }, 1)
      }
    | Error(msg) => Js.log(msg)
    }->Js.Promise.resolve
  }, _)

  /*
  arr => {
      let raw = Rom.new(arr->Uint8Array.fromBuffer)
      switch raw {
      | Ok(rom) => {
          let cpu = Cpu.new(Bus.new(rom))
          let canvasEl = document->Document.getElementById("canvas")->unwrapUnsafely
          let frame =
            canvasEl
            ->Canvas.CanvasElement.getContext2d
            ->C2d.createImageDataCoords(~width=32., ~height=32.)
          Cpu.reset(cpu)
          let break = ref(false)
          let _ = setInterval(() => {
            Cpu.step(
              cpu,
              [
                Screen.draw(canvasEl, frame),
                cpu => Cpu.mem_write(cpu, 0xfe, Js.Math.random_int(1, 16)),
                Debug.debug,
              ],
              break,
            )
          }, 1)
        }

      | Error(msg) => Js.log(msg)
      }
      a->Js.Promise.resolve
    }, _)
    ->Js.Promise.catch(err => {
      Js.log(err)
    }*/
}

let _ = {
  let romEl = document->Document.getElementById("rom")->unwrapUnsafely
  romEl->Element.addEventListener("change", main)
}
