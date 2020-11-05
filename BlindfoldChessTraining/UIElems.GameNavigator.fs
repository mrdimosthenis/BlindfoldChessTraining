module BlindfoldChessTraining.UIElems.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining

let notation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let a = View.Button(
                text = "a",
                textTransform = TextTransform.None,
                horizontalOptions = LayoutOptions.Center
                //fontSize = FontSize.fromValue model.ConfigOptions.FontSize
                //horizontalTextAlignment = TextAlignment.Center,
                //verticalTextAlignment = TextAlignment.Center
            )
    View.FlexLayout(
        horizontalOptions = LayoutOptions.Center,
        wrap = FlexWrap.Wrap,
        children = List.replicate 100 a
    )
