module BlindfoldChessTraining.Page.Home

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.ScrollView(
            content = View.StackLayout(
                horizontalOptions = LayoutOptions.Center,
                verticalOptions = LayoutOptions.Center,
                children = [
                    View.Label(
                        text = "Blindfold Chess Training",
                        fontSize = FontSize.fromValue (Constants.titleSizeRatio * model.ConfigOptions.FontSize),
                        fontAttributes = FontAttributes.Bold,
                        horizontalOptions = LayoutOptions.Center,
                        verticalOptions = LayoutOptions.Start
                    )
                    View.Button(text = "Opening Puzzles", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.OpeningPuzzlesPage))
                    View.Button(text = "Endgame Puzzles", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.EndgamePuzzlesPage))
                    View.Button(text = "Description", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.DescriptionPage))
                    View.Button(
                        text = "Options",
                        horizontalOptions = LayoutOptions.Center,
                        command = (fun () -> dispatch (Msg.SelectPage Model.OptionsPage)),
                        image = Icons.options
                    )
                    View.Button(
                        text = "Credits",
                        horizontalOptions = LayoutOptions.Center,
                        command = (fun () -> dispatch (Msg.SelectPage Model.CreditsPage))
                    )
                ]
            )
        )
    )
