module BlindfoldChessTraining.UIElems.Page

open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let template model title (icon: WidgetFabImage) (innerElems: WidgetFabLayout list) =
    let backBtn =
        match model.SelectedPage with
        | HomePage -> Icons.exit BackPressed
        | _ -> Icons.play_left BackPressed

    let label = Components.label model.ConfigOptions.FontSizeRatio title

    let header =
        Grid(rowdefs = [ Auto ], coldefs = [ Auto; Star; Auto ]) {
            backBtn.gridColumn 0
            label.gridColumn 1
            icon.gridColumn 2
        }

    ContentPage(
        ScrollView(
            (VStack(spacing = 25.) {
                header

                for el in innerElems do
                    match el with
                    | HorizSt layout -> layout
                    | VertSt layout -> layout
                    | Flx layout -> layout
                    | Grd layout -> layout
                    | Lbl lbl -> lbl
                    | Btn btn -> btn
                    | Img img -> img
            })
                .padding (30., 30., 30., 30.)
        )
    )
