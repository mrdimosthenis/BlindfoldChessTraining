module BlindfoldChessTraining.UIElems.Icons

open Fabulous.Maui
open type Fabulous.Maui.View

let empty () = Image("icons/empty.png")

let home () = Image("icons/home.png")

let library () = Image("icons/library.png")

let annotation: Fabulous.WidgetBuilder<obj, IFabImage> =
    Image("icons/annotation.png")

let fingerprint: Fabulous.WidgetBuilder<obj, IFabImage> =
    Image("icons/fingerprint.png")

let cube () = Image("icons/cube.png")

let options () = Image("icons/options.png")

let speaker () = Image("icons/speaker.png")

let chip () = Image("icons/chip.png")

let code () = Image("icons/code.png")
let idCard () = Image("icons/id_card.png")

let eye () = Image("icons/eye.png")

let volume_up () = Image("icons/volume_up.png")

let arrowCircleLeft () = Image("icons/arrow_circle_left.png")
let arrowCircleRight () = Image("icons/arrow_circle_right.png")

let fastForward () = Image("icons/fast_forward.png")
let rewind () = Image("icons/rewind.png")

let chevronLeft () = Image("icons/chevron_left.png")
let chevronRight () = Image("icons/chevron_right.png")

let chevronDoubleLeft () = Image("icons/chevron_double_left.png")
let chevronDoubleRight () = Image("icons/chevron_double_right.png")

let document () = Image("icons/document.png")
let share () = Image("icons/share.png")
let info () = Image("icons/info.png")
let play () = Image("icons/play.png")

let play_left () = Image("icons/play_left.png")
let exit () = Image("icons/exit.png")

let list () = Image("icons/list.png")
