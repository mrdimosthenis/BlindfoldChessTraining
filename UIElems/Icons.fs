﻿module BlindfoldChessTraining.UIElems.Icons

open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let mainLogo: WidgetFabImage = Image("logos/main")

let empty msg = Components.btnIcon "icons/empty" msg

let home: WidgetFabImage = Image("icons/home")
let homeColored: WidgetFabImage = Image("icons/home_colored")

let cube text msg =
    Components.btnIconText "icons/cube" text msg

let cubeColored: WidgetFabImage = Image("icons/cube_colored")

let library text msg =
    Components.btnIconText "icons/library" text msg

let libraryColored: WidgetFabImage = Image("icons/library_colored")

let annotation text msg =
    Components.btnIconText "icons/annotation" text msg

let annotationColored: WidgetFabImage = Image("icons/annotation_colored")

let options text msg =
    Components.btnIconText "icons/options" text msg

let optionsColored: WidgetFabImage = Image("icons/annotation_colored")

let fingerprint text msg =
    Components.btnIconText "icons/fingerprint" text msg

let fingerprintColored: WidgetFabImage = Image("icons/fingerprint_colored")

let eye text msg =
    Components.btnIconText "icons/eye" text msg

let speaker text msg =
    Components.btnIconText "icons/speaker" text msg

let chip text msg =
    Components.btnIconText "icons/chip" text msg

let share text msg =
    Components.btnIconText "icons/share" text msg

let star text msg =
    Components.btnIconText "icons/star" text msg

let code text msg =
    Components.btnIconText "icons/code" text msg

let idCard text msg =
    Components.btnIconText "icons/id_card" text msg

let document text msg =
    Components.btnIconText "icons/document" text msg

let arrowCircleLeft msg =
    Components.btnIcon "icons/arrow_circle_left" msg

let arrowCircleRight msg =
    Components.btnIcon "icons/arrow_circle_right" msg

let fastForward msg =
    Components.btnIcon "icons/fast_forward" msg

let rewind msg = Components.btnIcon "icons/rewind" msg

let chevronLeft msg =
    Components.btnIcon "icons/chevron_left" msg

let chevronRight msg =
    Components.btnIcon "icons/chevron_right" msg

let chevronDoubleLeft msg =
    Components.btnIcon "icons/chevron_double_left" msg

let chevronDoubleRight msg =
    Components.btnIcon "icons/chevron_double_right" msg

let play_left msg =
    Components.btnIcon "icons/play_left" msg

let exit msg = Components.btnIcon "icons/exit" msg