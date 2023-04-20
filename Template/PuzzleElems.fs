module BlindfoldChessTraining.Template.PuzzleElems

open Fabulous.Maui
open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open type Fabulous.Maui.View

let levelNavigation model =
    let level = model.CurrentGame.Level
    let levelMainText = $"Level {level + 1}"

    let levelParenText =
        match model.CurrentGame.CategoryId with
        | 0 -> $"({level + 5} Pieces)"
        | 1 -> $"({level + 10} Half Moves)"
        | _ -> raise WrongCategoryId

    (HStack() {
        UIElems.Icons.rewind (GoToMsg PrevLevel)

        UIElems.Components.label model.ConfigOptions.FontSizeRatio $"{levelMainText} {levelParenText}"

        UIElems.Icons.fastForward (GoToMsg NextLevel)
    })
        .centerHorizontal ()

let puzzleNavigation model =
    (HStack() {
        UIElems.Icons.arrowCircleLeft (GoToMsg PrevPuzzle)

        UIElems.Components.label model.ConfigOptions.FontSizeRatio $"Puzzle {model.CurrentGame.IndexInLevel + 1}"

        UIElems.Icons.arrowCircleRight (GoToMsg NextPuzzle)
    })
        .centerHorizontal ()

let piecesDescription model =
    let fontSizeRatio = model.ConfigOptions.FontSizeRatio

    let { IsWhiteToMove = isWhiteToMove
          WhitePieces = whitePieces
          BlackPieces = blackPieces } =
        model.CurrentGame

    let sideToPlay, moveSideDescription, otherSideDescription, moveSidePieceDescription, otherSidePieceDescription =
        if isWhiteToMove then
            ("White to Play",
             "White Pieces",
             "Black Pieces",
             String.concat ", " whitePieces,
             String.concat ", " blackPieces)
        else
            ("Black to Play",
             "Black Pieces",
             "White Pieces",
             String.concat ", " blackPieces,
             String.concat ", " whitePieces)

    VStack() {
        UIElems.Components.label fontSizeRatio sideToPlay
        UIElems.Components.label fontSizeRatio $"{moveSideDescription}: {moveSidePieceDescription}"
        UIElems.Components.label fontSizeRatio $"{otherSideDescription}: {otherSidePieceDescription}"
    }

let boardOption model =
    (HStack() {
        CheckBox(model.IsDisplayBoardEnabled, (fun _ -> SwitchIsDisplayBoardEnabled))

        UIElems.Components.label model.ConfigOptions.FontSizeRatio "Chessboard"
    })
        .centerHorizontal ()
