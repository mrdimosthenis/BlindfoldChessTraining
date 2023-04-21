module BlindfoldChessTraining.UIElems.Page

open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let template model title icon innerElems =
    let backBtn =
        match model.SelectedPage with
        | HomePage -> Icons.exit BackPressed
        | _ -> Icons.play_left BackPressed

    let header =
        (HStack() {
            backBtn
            Components.label model.ConfigOptions.FontSizeRatio title
            icon
        })
            .centerHorizontal ()

    (VStack() {
        header

        for el in innerElems do
            el
    })
        .centerHorizontal ()
