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
            horizontalOptions = LayoutOptions.FillAndExpand,
            verticalOptions = LayoutOptions.Center,
            children = [
                
                Board.boardGrid 0.95 true Board.init
                
                View.Label(text = "Blindfold Chess Training", fontAttributes = FontAttributes.Bold, horizontalOptions = LayoutOptions.Center)
                View.Button(text = "Opening Puzzles", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.OpeningPuzzlesPage))
                View.Button(text = "Endgame Puzzles", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.EndgamePuzzlesPage))
                View.Button(text = "Description", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.DescriptionPage))
                View.Button(text = "Credits", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.CreditsPage))
                View.Label(text = "version 3.0.0", fontAttributes = FontAttributes.Italic, horizontalOptions = LayoutOptions.End)
            ]
        )
    )
