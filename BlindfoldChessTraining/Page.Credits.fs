module BlindfoldChessTraining.Page.Credits

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(
            verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(text = "Credits", fontAttributes = FontAttributes.Bold, horizontalOptions = LayoutOptions.Center)
                View.Button(text = "Back", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.HomePage))
            ]
        )
    )
