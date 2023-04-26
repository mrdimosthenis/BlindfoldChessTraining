module BlindfoldChessTraining.Pages.OpeningPuzzles

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
          FontSizeRatio = fontSizeRatio
          BoardSizeRatio = boardSizeRatio } =
        configOptions


    let innerElems =
        [ model |> UIElems.PuzzleElems.levelNavigation |> HorizSt
          model |> UIElems.PuzzleElems.puzzleNavigation |> HorizSt

          "First Moves" |> UIElems.Components.label fontSizeRatio |> Lbl
          model |> UIElems.PuzzleElems.notation |> Flx

          if not isPuzzleSolved && Constants.os = Android then
              model |> UIElems.PuzzleElems.boardOption |> HorizSt

          if (isDisplayBoardEnabled || Constants.os = IOS) && not isPuzzleSolved then
              Logic.Board.empty
              |> UIElems.Board.boardGrid areCoordsEnabled boardSizeRatio
              |> Grd

          if isPuzzleSolved then
              model |> UIElems.Board.grid |> Grd
              model |> UIElems.PuzzleElems.boardNavigation |> HorizSt
          else
              ShowSolution |> UIElems.Icons.eye "Solution" |> Btn

          if not didVolumeNoteClicked then
              model |> UIElems.PuzzleElems.speechNotification |> Lbl ]

    UIElems.Page.template model "Opening Puzzles" UIElems.Icons.libraryColored innerElems
