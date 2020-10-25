module BlindfoldChessTraining.Page.Home

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems

open BlindfoldChessMechanics.Logic

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(
            horizontalOptions = LayoutOptions.Center,
            verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(
                    text = "Blindfold Chess Training",
                    fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                    fontAttributes = FontAttributes.Bold,
                    horizontalOptions = LayoutOptions.Center
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
                View.Label(
                    text = "version 3.0.0",
                    fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                    fontAttributes = FontAttributes.Italic,
                    horizontalOptions = LayoutOptions.End
                )
            ]
        )
    )
