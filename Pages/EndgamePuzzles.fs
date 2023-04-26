module BlindfoldChessTraining.Pages.EndgamePuzzles

open BlindfoldChessMechanics
open BlindfoldChessTraining
open BlindfoldChessTraining.Types

let view model =

    let { IsDisplayBoardEnabled = isDisplayBoardEnabled
          ConfigOptions = configOptions
          IsPuzzleSolved = isPuzzleSolved
          DidVolumeNoteClicked = didVolumeNoteClicked } =
        model

    let { AreCoordsEnabled = areCoordsEnabled
          BoardSizeRatio = boardSizeRatio } =
        configOptions


    let innerElems =
        [ model |> UIElems.PuzzleElems.levelNavigation |> HorizSt
          model |> UIElems.PuzzleElems.puzzleNavigation |> HorizSt

          model |> UIElems.PuzzleElems.piecesDescription |> VertSt

          if not isPuzzleSolved && Constants.os = Android then
              model |> UIElems.PuzzleElems.boardOption |> HorizSt

          if (isDisplayBoardEnabled || Constants.os = IOS) && not isPuzzleSolved then
              Logic.Board.empty
              |> UIElems.Board.boardGrid areCoordsEnabled boardSizeRatio
              |> Grd

          if isPuzzleSolved then
              model |> UIElems.PuzzleElems.notation |> Flx
              model |> UIElems.Board.grid |> Grd
          else
              ShowSolution |> UIElems.Icons.eye "Solution" |> Btn

          if not didVolumeNoteClicked then
              model |> UIElems.PuzzleElems.speechNotification |> Lbl ]

    UIElems.Page.template model "Endgame Puzzles" UIElems.Icons.cubeColored innerElems
