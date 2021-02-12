module BlindfoldChessTraining.Page.OpeningPuzzles

open Fabulous

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template
open BlindfoldChessMechanics

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let innerElems =
        [ [ GameNavigator.levelNavigation model dispatch ]
          [ Component.separator () ]
          [ Component.label model false "First Moves" ]
          [ GameNavigator.notation model ]
          if model.IsPuzzleSolved then [] else [ Component.separator () ]
          if model.IsPuzzleSolved || Constants.isIOSDevice
          then []
          else [ GameNavigator.displayBoardOption model dispatch ]
          if (model.IsDisplayBoardOptionEnabled
              || Constants.isIOSDevice)
             && not model.IsPuzzleSolved then
              [ Logic.Board.init
                |> UIElems.Board.grid dispatch model.ConfigOptions.AreCoordsEnabled model.ConfigOptions.BoardSize ]
          else
              []
          if model.IsPuzzleSolved
          then [ GameNavigator.chessboard model dispatch ]
          else []
          if not model.IsPuzzleSolved
          then [ Component.button "Solution" Icons.eye false (fun () -> dispatch Msg.ShowSolution) ]
          else []
          if model.IsPuzzleSolved
          then [ GameNavigator.boardNavigation model dispatch ]
          else []
          if model.DidVolumeNoteClicked then
              []
          else
              [ Component.separator ()
                GameNavigator.volumeNotification model dispatch ] ]
        |> List.concat

    Template.Page.page model dispatch "Opening Puzzles" Icons.library innerElems
