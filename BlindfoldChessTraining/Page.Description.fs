module BlindfoldChessTraining.Page.Description

open Fabulous

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

let audioTrainingDescription: string =
    if Constants.isIOSDevice then
        "Swipe left and right to listen to the puzzle description. Swipe right describes the puzzle sentence by sentence. Swipe left starts from the top."
    else
        "Press the volume keys on your device to listen to the puzzle description. Volume down key describes the puzzle sentence by sentence, while volume up key starts from the top."

let description: string =
    sprintf """Improve your blindfold thinking by solving chess puzzles without diagrams. Read or listen to each puzzle description and try to find the best move. Train yourself while walking, jogging or in transportation.

In Opening Puzzles try to visualize the position that arises after the first moves. In Endgame Puzzles you can do that by noticing the coordinates of the pieces.

1000 puzzles organized according to the number of half moves or of the pieces on the chessboard. All puzzles are fairly easy to solve even for a beginner.

%s""" audioTrainingDescription


let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    [ Component.label model false description ]
    |> Page.page model dispatch "Description" Icons.annotation
