module BlindfoldChessTraining.Template.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template
open BlindfoldChessMechanics
open FSharpx.Collections
open System

let levelNavigation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let level = model.CurrentGame.Level
    let levelMainText = level + 1 |> sprintf "Level %i"
    let levelParenText = match model.SelectedPage with
                         | Model.OpeningPuzzlesPage -> sprintf "(%i Half Moves)" (level + 10)
                         | _ -> sprintf "(%i Pieces)" (level + 5)
    let levelInfo =
            View.StackLayout(
                orientation = StackOrientation.Horizontal,
                horizontalOptions = LayoutOptions.Center,
                children = [
                    View.Button(
                        image = Icons.rewind,
                        command = (fun () -> dispatch Msg.GoToPrevLevel)
                    )
                    levelMainText + " " + levelParenText |> Component.label model false
                    View.Button(
                        image = Icons.fastForward,
                        command = (fun () -> dispatch Msg.GoToNextLevel)
                    )
                ]
            )
    let puzzleInfo =
            View.StackLayout(
                orientation = StackOrientation.Horizontal,
                horizontalOptions = LayoutOptions.Center,
                children = [
                    View.Button(
                        image = Icons.arrowCircleLeft,
                        command = (fun () -> dispatch Msg.GoToPrevPuzzle)
                    )
                    model.CurrentGame.IndexInLevel + 1 |> sprintf "Puzzle %i" |> Component.label model false
                    View.Button(
                        image = Icons.arrowCircleRight,
                        command = (fun () -> dispatch Msg.GoToNextPuzzle)
                    )
                ]
            )
    View.StackLayout(
        horizontalOptions = LayoutOptions.Center,
        verticalOptions = LayoutOptions.Center,
        children = [ levelInfo; puzzleInfo ]
    )

let pieceDescriptions (model: Model.Model): ViewElement =
    let isWhiteToMove = model.CurrentGame.IsWhiteToMove
    let whitePieces = model.CurrentGame.WhitePieces
    let blackPieces = model.CurrentGame.BlackPieces
    let (sideToPlay, moveSideDescription, otherSideDescription, moveSidePieceDescription, otherSidePieceDescription) =
            if isWhiteToMove
                then ("White to play", "White pieces", "Black pieces", String.Join(",", whitePieces), String.Join(",", blackPieces))
                else ("Black to play", "Black pieces", "White pieces", String.Join(",", blackPieces), String.Join(",", whitePieces))
    View.StackLayout(
        horizontalOptions = LayoutOptions.Center,
        verticalOptions = LayoutOptions.Center,
        children = [ Component.label model false sideToPlay
                     sprintf "%s: %s" moveSideDescription  moveSidePieceDescription |> Component.label model false
                     sprintf "%s: %s" otherSideDescription  otherSidePieceDescription |> Component.label model false ]
    )

let displayBoardOption (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.StackLayout(
        orientation = StackOrientation.Horizontal,
        horizontalOptions = LayoutOptions.Center,
        children = [
            View.CheckBox(
                isChecked = model.IsDisplayBoardOptionEnabled,
                checkedChanged = (fun args -> model.IsDisplayBoardOptionEnabled |> not |> Msg.SelectDisplayBoardOption |> dispatch)
            )
            Component.label model false "Chessboard"
        ]
    )

let chessboard (model: Model.Model): ViewElement =
    match model.CurrentMoveIndex with
    | None -> model.CurrentGame.InitBoard
    | Some i -> model.CurrentGame.Boards.[i]
    |> UIElems.Board.grid model.ConfigOptions.AreSymbolsEnabled

let notation (model: Model.Model): ViewElement =
    let flexChildren = LazyList.fold
                             (fun (accI, accLaz) (s, b) ->
                                 let nextAccI = if b then accI else accI + 1
                                 let currentMoveIndex = if b then None else Some accI
                                 let nextAccLaz = Utils.prependedLaz (currentMoveIndex, s, b) accLaz
                                 (nextAccI, nextAccLaz)
                             )
                             (0, LazyList.empty)
                             model.CurrentGame.MovesWithNumberIndicators
                       |> snd
                       |> (if model.IsPuzzleSolved then id else LazyList.tail)
                       |> LazyList.rev
                       |> LazyList.map
                             (fun (iOpt, s, b) ->
                                 View.Label(
                                     text = (if b then s else s + " "),
                                     fontAttributes = (if iOpt = model.CurrentMoveIndex then FontAttributes.Bold else FontAttributes.None),
                                     fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                     textColor = Constants.textColor,
                                     horizontalTextAlignment = TextAlignment.Center,
                                     verticalTextAlignment = TextAlignment.Center,
                                     horizontalOptions = LayoutOptions.Center,
                                     verticalOptions = LayoutOptions.Center
                                 )
                             )
                       |> LazyList.toList
    View.FlexLayout(
        justifyContent = FlexJustify.Center,
        wrap = FlexWrap.Wrap,
        children = flexChildren
    )

let boardNavigation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.StackLayout(
        orientation = StackOrientation.Horizontal,
        horizontalOptions = LayoutOptions.Center,
        children = [
            View.Button(
                image = (if model.CurrentMoveIndex.IsSome then Icons.chevronDoubleLeft else Icons.empty),
                command = (fun () -> dispatch Msg.GoToInitPos)
            )
            View.Button(
                image = (if model.CurrentMoveIndex.IsSome then Icons.chevronLeft else Icons.empty),
                command = (fun () -> dispatch Msg.GoToPrevMove)
            )
            View.Button(
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGame.Boards.Length - 1)
                         then Icons.chevronRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToNextMove)
            )
            View.Button(
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGame.Boards.Length - 1)
                         then Icons.chevronDoubleRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToLastPos)
            )
        ]
    )
