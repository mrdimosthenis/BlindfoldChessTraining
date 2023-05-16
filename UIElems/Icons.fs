module BlindfoldChessTraining.UIElems.Icons

open System
open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let webImg src : WidgetFabImage = Image(Uri src)

let mainLogo: WidgetFabImage = Image("main.png")

let emptyImg: WidgetFabImage = Image("empty.png")

let emptyBtn msg =
    (Components.btnIcon "empty.png" msg).opacity 0

let homeColored: WidgetFabImage = Image("home_colored.png")

let cube text msg =
    Components.btnIconText "cube.png" text msg

let cubeColored: WidgetFabImage = Image("cube_colored.png")

let library text msg =
    Components.btnIconText "library.png" text msg

let libraryColored: WidgetFabImage = Image("library_colored.png")

let annotation text msg =
    Components.btnIconText "annotation.png" text msg

let annotationColored: WidgetFabImage = Image("annotation_colored.png")

let options text msg =
    Components.btnIconText "options.png" text msg

let optionsColored: WidgetFabImage = Image("options_colored.png")

let fingerprint text msg =
    Components.btnIconText "fingerprint.png" text msg

let fingerprintColored: WidgetFabImage = Image("fingerprint_colored.png")

let eye text msg =
    Components.btnIconText "eye.png" text msg

let speaker text msg =
    Components.btnIconText "speaker.png" text msg

let chip text msg =
    Components.btnIconText "chip.png" text msg

let share text msg =
    Components.btnIconText "share.png" text msg

let star text msg =
    Components.btnIconText "star.png" text msg

let code text msg =
    Components.btnIconText "code.png" text msg

let idCard text msg =
    Components.btnIconText "id_card.png" text msg

let document text msg =
    Components.btnIconText "document.png" text msg

let arrowCircleLeft msg =
    Components.btnIcon "arrow_circle_left.png" msg

let arrowCircleRight msg =
    Components.btnIcon "arrow_circle_right.png" msg

let fastForward msg =
    Components.btnIcon "fast_forward.png" msg

let rewind msg = Components.btnIcon "rewind.png" msg

let chevronLeft msg =
    Components.btnIcon "chevron_left.png" msg

let chevronRight msg =
    Components.btnIcon "chevron_right.png" msg

let chevronDoubleLeft msg =
    Components.btnIcon "chevron_double_left.png" msg

let chevronDoubleRight msg =
    Components.btnIcon "chevron_double_right.png" msg

let play_left msg = Components.btnIcon "play_left.png" msg

let exit msg = Components.btnIcon "exit.png" msg
