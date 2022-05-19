open Js.TypedArray2
open Webapi.Dom
module Evt = Webapi.Dom.Event
module Canvas = Webapi.Canvas
module CElem = Webapi.Canvas.CanvasElement
module C2d = Webapi.Canvas.Canvas2d
%%raw(`import "../../../node_modules/diff/dist/diff.js";`)
let unwrapUnsafely = x =>
  switch x {
  | Some(v) => v
  | None => raise(Invalid_argument("Passed `None` to unwrapUnsafely"))
  }
let canvasEl = document->Document.getElementById("canvas")->unwrapUnsafely
let startBtn = document->Document.getElementById("start")->unwrapUnsafely
let resetBtn = document->Document.getElementById("reset")->unwrapUnsafely
let stepBtn = document->Document.getElementById("step")->unwrapUnsafely
let debugBox = document->Document.getElementById("debug")->unwrapUnsafely

type intervalID = int
@val external setInterval: (unit => unit, int) => intervalID = "setInterval"
@val external clearInterval: intervalID => unit = "clearInterval"
@get external getFiles: Dom.eventTarget => Webapi.FileList.t = "files"
let enableBtn = btn => {
  let _ = btn->Element.removeAttribute("disabled")
}
let disableBtn = btn => {
  let _ = btn->Element.setAttribute("disabled", "")
}
let loadBinaryData = raw => {
  let rom = Rom.new(raw->Uint8Array.fromBuffer)
  Belt.Result.getExn(rom)
}
let diffLog = %raw(`
function(one, other){
  const diff = Diff.diffLines(one, other),
      display = document.getElementById('display'),
      fragment = document.createDocumentFragment();
  diff.forEach((part) => {
  // green for additions, red for deletions
  // grey for common parts
    const color = part.added ? 'green' :
      part.removed ? 'red' : 'grey';
    let span = document.createElement('span');
    span.style.color = color;
    if(!part.added && !part.removed){
      part.value = part.value.slice(part.value.lastIndexOf('\n', part.value.length - 2)+1)
    }
    span.appendChild(document
      .createTextNode(part.value));
    fragment.appendChild(span);
  });
  display.appendChild(fragment);
}
`)
let runRom = rom => {
  let cpu = Cpu.new(Bus.new(rom))
  let break = ref(false)
  let _ = startBtn->disableBtn
  let _ = resetBtn->enableBtn
  let _ = debugBox->disableBtn
  let frame =
    canvasEl->Canvas.CanvasElement.getContext2d->C2d.createImageDataCoords(~width=32., ~height=32.)
  Cpu.reset(cpu)
  cpu.pc = 0xC000
  let log = ref("")
  try {
    cpu->Cpu.run_with_callback([cpu => log := log.contents ++ Debug.trace(cpu)])
  } catch {
  | e => Js.log(e)
  }
  let _ = diffLog(log.contents, Log.expected)
  /*
  setInterval(() => {
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
*/
  1
}
let reset = id => {
  debugBox->enableBtn
  clearInterval(id)
  let _ =
    canvasEl
    ->Canvas.CanvasElement.getContext2d
    ->C2d.clearRect(
      ~x=0.,
      ~y=0.,
      ~w=canvasEl->Canvas.CanvasElement.width->float_of_int,
      ~h=canvasEl->Canvas.CanvasElement.height->float_of_int,
    )
  let _ = startBtn->enableBtn
}
@get external checked: Element.t => bool = "checked"
let debugMode = (rom, _) => {
  if debugBox->checked {
    startBtn->disableBtn
    resetBtn->enableBtn
    stepBtn->enableBtn
    let cpu = Cpu.new(Bus.new(rom))
    let break = ref(false)
    let frame =
      canvasEl
      ->Canvas.CanvasElement.getContext2d
      ->C2d.createImageDataCoords(~width=32., ~height=32.)
    cpu->Cpu.reset
    let () = stepBtn->Element.setOnClick(_ => {
      Cpu.step(
        cpu,
        [
          cpu => Cpu.mem_write(cpu, 0xfe, Js.Math.random_int(1, 16)),
          Debug.debug,
          Screen.draw(canvasEl, frame),
        ],
        break,
      )
    })
  } else {
    startBtn->enableBtn
    resetBtn->enableBtn
    stepBtn->disableBtn
    ()
  }
}
let onRomUploaded = changeEvent => {
  let fileObj =
    changeEvent
    ->Event.currentTarget
    ->getFiles
    ->Webapi.FileList.item(0)
    ->Belt.Option.getExn
    ->Webapi.File.arrayBuffer
  let _ = fileObj->Js.Promise.then_(arr => {
    let rom = loadBinaryData(arr)
    startBtn->enableBtn
    resetBtn->enableBtn
    debugBox->enableBtn
    startBtn->Element.setOnClick(_ => {
      let id = runRom(rom)
      resetBtn->Element.setOnClick(_ => reset(id))
    })
    debugBox->Element.addEventListener("change", debugMode(rom))
    Js.Promise.resolve()
  }, _)
}

let _ = {
  let romEl = document->Document.getElementById("rom")->unwrapUnsafely
  let _ = romEl->Element.addEventListener("change", onRomUploaded)
}
