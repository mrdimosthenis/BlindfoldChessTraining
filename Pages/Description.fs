module BlindfoldChessTraining.Pages.Description

open BlindfoldChessTraining
open BlindfoldChessTraining.Types

let view model =
    [ "Improve your blindfold thinking by solving chess puzzles without diagrams. Read or listen to each puzzle description and try to find the best move. Train yourself while walking, jogging or in transportation."
      "In Opening Puzzles try to visualize the position that arises after the first moves. In Endgame Puzzles you can do that by noticing the coordinates of the pieces."
      "1000 puzzles organized according to the number of half moves or of the pieces on the chessboard. All puzzles are fairly easy to solve even for a beginner."
      match Constants.os with
      | Android ->
          "Press the volume keys on your device (or swipe left and right on the chessboard) to listen to the puzzle description. Volume down key (or left swipe) describes the puzzle sentence by sentence. Volume up key (or right swipe) starts from the top."
      | IOS ->
          "Swipe left and right on the chessboard to listen to the puzzle description. Left swipe describes the puzzle sentence by sentence. Right swipe starts from the top." ]
    |> List.map (UIElems.Components.label model.ConfigOptions.FontSizeRatio )
    |> List.map Lbl
    |> UIElems.Page.template model "Description" UIElems.Icons.annotationColored
