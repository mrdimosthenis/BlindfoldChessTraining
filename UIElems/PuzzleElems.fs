module BlindfoldChessTraining.UIElems.PuzzleElems

open Fabulous.Maui
open FSharpx.Collections
open BlindfoldChessMechanics
open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts
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
        Icons.rewind (GoToMsg PrevLevel)

        Components.label model.ConfigOptions.FontSizeRatio $"{levelMainText} {levelParenText}"

        Icons.fastForward (GoToMsg NextLevel)
    })
        .centerHorizontal ()

let puzzleNavigation model =
    (HStack() {
        Icons.arrowCircleLeft (GoToMsg PrevPuzzle)

        Components.label model.ConfigOptions.FontSizeRatio $"Puzzle {model.CurrentGame.IndexInLevel + 1}"

        Icons.arrowCircleRight (GoToMsg NextPuzzle)
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
        Components.label fontSizeRatio sideToPlay
        Components.label fontSizeRatio $"{moveSideDescription}: {moveSidePieceDescription}"
        Components.label fontSizeRatio $"{otherSideDescription}: {otherSidePieceDescription}"
    }

let boardOption model =
    (HStack() {
        CheckBox(model.IsDisplayBoardEnabled, (fun _ -> SwitchIsDisplayBoardEnabled))

        Components.label model.ConfigOptions.FontSizeRatio "Chessboard"
    })
        .centerHorizontal ()

let notation model =
    let flexChildren =
        LazyList.fold
            (fun (accI, accLaz) (s, b) ->
                let nextAccI = if b then accI else accI + 1
                let currentMoveIndex = if b then None else Some accI

                let nextAccLaz = Utils.prependedLaz (currentMoveIndex, s, b) accLaz

                (nextAccI, nextAccLaz))
            (0, LazyList.empty)
            model.CurrentGame.MovesWithNumberIndicators
        |> snd
        // remove last move
        |> if model.IsPuzzleSolved then id else LazyList.tail
        // remove last number indicator if exist
        |> (fun laz ->
            let _, _, b = LazyList.head laz

            if b && not model.IsPuzzleSolved then
                LazyList.tail laz
            else
                laz)
        |> LazyList.rev
        |> LazyList.map (fun (iOpt, s, b) ->
            let txt = if b then s else s + " "

            let fontAttributes =
                if iOpt = model.CurrentMoveIndex then
                    FontAttributes.Bold
                else
                    FontAttributes.None

            Label(txt)
                .font (size = model.ConfigOptions.FontSizeRatio * Constants.fontSize, attributes = fontAttributes))

    (FlexLayout(FlexWrap.Wrap) {
        for lbl in flexChildren do
            lbl.centerVertical ()
    })
        .alignItems(FlexAlignItems.Center)
        .centerHorizontal ()

let boardNavigation model =
    let noOpIcon = Icons.emptyBtn NoOp

    let doubleLeftIcon, leftIcon =
        match model.CurrentMoveIndex with
        | None -> noOpIcon, noOpIcon
        | Some _ -> Icons.chevronDoubleLeft (GoToMsg InitPos), Icons.chevronLeft (GoToMsg PrevPos)

    let rightIcon, doubleRightIcon =
        match model.CurrentMoveIndex with
        | Some i when i = model.CurrentGame.Boards.Length - 1 -> noOpIcon, noOpIcon
        | _ -> Icons.chevronRight (GoToMsg NextPos), Icons.chevronDoubleRight (GoToMsg LastPos)

    (HStack() {
        doubleLeftIcon
        leftIcon
        rightIcon
        doubleRightIcon
    })
        .centerHorizontal ()

let speechNotification model =
    let txt =
        match Constants.os with
        | Android ->
            "Press the volume keys of the device (or swipe left and right on the chessboard) for audio training"
        | IOS -> "Swipe left and right on the chessboard for audio training"

    Label(txt)
        .alignEndText()
        .font(size = model.ConfigOptions.FontSizeRatio * Constants.fontSize, attributes = FontAttributes.Italic)
        .alignEndHorizontal ()
