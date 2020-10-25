module BlindfoldChessTraining.Page.Description

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

open BlindfoldChessTraining

let description: string =
    """Improve your blindfold thinking by solving chess puzzles without diagrams. Read or listen to each puzzle description and try to find the best move. Train yourself while walking, jogging or in transportation.

In Opening Puzzles try to visualize the position that arises after the first moves. In Endgame Puzzles you can do that by noticing the coordinates of the pieces.

1000 puzzles organized according to the number of half moves or of the pieces on the chessboard. All puzzles are fairly easy to solve even for a beginner.

Use the volume keys on your device to listen to the puzzle description. Volume down key describes the puzzle sentence by sentence, while volume up key starts over the top."""

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(
            verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(
                    text = "Description",
                    fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                    fontAttributes = FontAttributes.Bold,
                    horizontalOptions = LayoutOptions.Center
                )
                View.ScrollView(
                  View.Label(
                    text = description,
                    fontSize = FontSize.fromValue model.ConfigOptions.FontSize
                  )
                )
                View.Button(text = "Back", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.HomePage))
            ]
        )
    )
