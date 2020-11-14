module BlindfoldChessTraining.Page.EndgamePuzzles

open Fabulous

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template
open BlindfoldChessMechanics

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let innerElems =
            [ [ GameNavigator.levelNavigation model dispatch ]
              [ Component.separator() ]
              [ GameNavigator.pieceDescriptions model ]
              if model.IsPuzzleSolved
                  then [ Component.separator() ]
                  else []
              if model.IsPuzzleSolved
                  then []
                  else [ GameNavigator.displayBoardOption model dispatch ]
              if model.IsDisplayBoardOptionEnabled && not model.IsPuzzleSolved
                  then [ Logic.Board.empty |> UIElems.Board.grid model.ConfigOptions.AreCoordsEnabled ]
                  else []
              if model.IsPuzzleSolved
                  then [ GameNavigator.notation model ]
                  else []
              if model.IsPuzzleSolved
                  then [ GameNavigator.chessboard model ]
                  else []
              if not model.IsPuzzleSolved
                  then [ Component.button "Solution" Icons.eye false (fun () -> dispatch Msg.ShowSolution) ]
                  else []
              if model.IsPuzzleSolved
                    then [ GameNavigator.boardNavigation model dispatch ]
                    else [] ]
            |> List.concat
    Template.Page.page model dispatch "Endgame Puzzles" Icons.cube innerElems
