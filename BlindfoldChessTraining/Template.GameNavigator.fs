﻿module BlindfoldChessTraining.Template.GameNavigator

open System
open FSharpx.Collections

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template
open BlindfoldChessMechanics

let levelNavigation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let level = model.CurrentGame.Level
    let levelMainText = level + 1 |> sprintf "Level %i"

    let levelParenText =
        match model.SelectedPage with
        | Model.OpeningPuzzlesPage -> sprintf "(%i Half Moves)" (level + 10)
        | _ -> sprintf "(%i Pieces)" (level + 5)

    let levelInfo =
        View.StackLayout
            (orientation = StackOrientation.Horizontal,
             horizontalOptions = LayoutOptions.Center,
             children =
                 [ Component.imageButton Icons.rewind (fun () -> dispatch Msg.GoToPrevLevel)
                   levelMainText + " " + levelParenText
                   |> Component.label model false
                   Component.imageButton Icons.fastForward (fun () -> dispatch Msg.GoToNextLevel) ])

    let puzzleInfo =
        View.StackLayout
            (orientation = StackOrientation.Horizontal,
             horizontalOptions = LayoutOptions.Center,
             children =
                 [ Component.imageButton Icons.arrowCircleLeft (fun () -> dispatch Msg.GoToPrevPuzzle)
                   model.CurrentGame.IndexInLevel + 1
                   |> sprintf "Puzzle %i"
                   |> Component.label model false
                   Component.imageButton Icons.arrowCircleRight (fun () -> dispatch Msg.GoToNextPuzzle) ])

    View.StackLayout
        (horizontalOptions = LayoutOptions.Center,
         verticalOptions = LayoutOptions.Center,
         children = [ levelInfo; puzzleInfo ])

let pieceDescriptions (model: Model.Model): ViewElement =
    let isWhiteToMove = model.CurrentGame.IsWhiteToMove
    let whitePieces = model.CurrentGame.WhitePieces
    let blackPieces = model.CurrentGame.BlackPieces

    let (sideToPlay, moveSideDescription, otherSideDescription, moveSidePieceDescription, otherSidePieceDescription) =
        if isWhiteToMove then
            ("White to Play",
             "White Pieces",
             "Black Pieces",
             String.Join(", ", whitePieces),
             String.Join(", ", blackPieces))
        else
            ("Black to Play",
             "Black Pieces",
             "White Pieces",
             String.Join(", ", blackPieces),
             String.Join(", ", whitePieces))

    View.StackLayout
        (horizontalOptions = LayoutOptions.Center,
         verticalOptions = LayoutOptions.Center,
         children =
             [ Component.label model false sideToPlay
               sprintf "%s: %s" moveSideDescription moveSidePieceDescription
               |> Component.label model false
               sprintf "%s: %s" otherSideDescription otherSidePieceDescription
               |> Component.label model false ])

let displayBoardOption (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         children =
             [ View.CheckBox
                 (isChecked = model.IsDisplayBoardOptionEnabled,
                  checkedChanged =
                      (fun args ->
                          model.IsDisplayBoardOptionEnabled
                          |> not
                          |> Msg.SelectDisplayBoardOption
                          |> dispatch))
               Component.label model false "Chessboard" ])

let chessboard (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    match model.CurrentMoveIndex with
    | None -> model.CurrentGame.InitBoard
    | Some i -> model.CurrentGame.Boards.[i]
    |> UIElems.Board.grid dispatch model.ConfigOptions.AreCoordsEnabled model.ConfigOptions.BoardSize

let notation (model: Model.Model): ViewElement =
    let flexChildren =
        LazyList.fold (fun (accI, accLaz) (s, b) ->
            let nextAccI = if b then accI else accI + 1
            let currentMoveIndex = if b then None else Some accI

            let nextAccLaz =
                Utils.prependedLaz (currentMoveIndex, s, b) accLaz

            (nextAccI, nextAccLaz)) (0, LazyList.empty) model.CurrentGame.MovesWithNumberIndicators
        |> snd
        // remove last move
        |> if model.IsPuzzleSolved then id else LazyList.tail
        // remove last number indicator if exist
        |> (fun laz ->
            let (_, _, b) = LazyList.head laz
            if b && not model.IsPuzzleSolved then LazyList.tail laz else laz)
        |> LazyList.rev
        |> LazyList.map (fun (iOpt, s, b) ->
            View.Label
                (text = (if b then s else s + " "),
                 fontAttributes = (if iOpt = model.CurrentMoveIndex then FontAttributes.Bold else FontAttributes.None),
                 fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                 textColor = Constants.textColor,
                 horizontalTextAlignment = TextAlignment.Center,
                 verticalTextAlignment = TextAlignment.Center,
                 horizontalOptions = LayoutOptions.Center,
                 verticalOptions = LayoutOptions.Center))
        |> LazyList.toList

    View.FlexLayout(justifyContent = FlexJustify.Center, wrap = FlexWrap.Wrap, children = flexChildren)

let boardNavigation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         children =
             [ Component.imageButton (if model.CurrentMoveIndex.IsSome then Icons.chevronDoubleLeft else Icons.empty) (fun () ->
                   dispatch Msg.GoToInitPos)

               Component.imageButton (if model.CurrentMoveIndex.IsSome then Icons.chevronLeft else Icons.empty) (fun () ->
                   dispatch Msg.GoToPrevMove)

               Component.imageButton
                   (if model.CurrentMoveIndex
                       <> Some(model.CurrentGame.Boards.Length - 1) then
                       Icons.chevronRight
                    else
                        Icons.empty) (fun () -> dispatch Msg.GoToNextMove)

               Component.imageButton
                   (if model.CurrentMoveIndex
                       <> Some(model.CurrentGame.Boards.Length - 1) then
                       Icons.chevronDoubleRight
                    else
                        Icons.empty) (fun () -> dispatch Msg.GoToLastPos) ])

let speechNotification (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let label =
        if Constants.isIOSDevice
        then "Swipe left and right on the chessboard for audio training"
        else "Press the volume keys of the device (or swipe left and right on the chessboard) for audio training"
        |> Component.label model false

    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         verticalOptions = LayoutOptions.Center,
         children =
             [ label
               Component.imageButton Icons.volume_up (fun () -> dispatch Msg.VolumeNoteClicked) ])
