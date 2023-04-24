module BlindfoldChessTraining.UIElems.Page

open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let template model title (icon: WidgetFabImage) (innerElems: WidgetFabLayout list) =
    let backBtn =
        match model.SelectedPage with
        | HomePage -> Icons.exit BackPressed
        | _ -> Icons.play_left BackPressed

    let header =
        (HStack() {
            backBtn.alignStartHorizontal()
            Components.label model.ConfigOptions.FontSizeRatio title
            icon.alignEndHorizontal()
        }).centerHorizontal()

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
                    | Lbl layout -> layout
            })
                .padding(30., 0., 30., 0.)
                .centerVertical ()
        )
    )
