module BlindfoldChessTraining.UIElems.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessMechanics
open FSharpx.Collections

let chessboard (model: Model.Model): ViewElement =
    match model.CurrentMoveIndex with
    | None -> model.CurrentGameWithBoards.InitBoard
    | Some i -> model.CurrentGameWithBoards.MovesWithBoards.[i] |> snd
    |> UIElems.Board.grid model.ConfigOptions.AreSymbolsEnabled

let notation (model: Model.Model): ViewElement =
    let movesWithIndicators = model.CurrentGameWithBoards.MovesWithBoards
                              |> LazyList.ofArray
                              |> LazyList.map fst
                              |> Notation.Emitter.moveTextsWithNumberIndicators
                                          model.ConfigOptions.AreSymbolsEnabled
                                          model.CurrentGameWithBoards.IsWhiteToMove
    let flexChildren = LazyList.fold
                             (fun (accI, accLaz) (s, b) ->
                                 let nextAccI = if b then accI else accI + 1
                                 let currentMoveIndex = if b then None else Some accI
                                 let nextAccLaz = Utils.prependedLaz (currentMoveIndex, s, b) accLaz
                                 (nextAccI, nextAccLaz)
                             )
                             (0, LazyList.empty)
                             movesWithIndicators
                       |> snd
                       |> LazyList.rev
                       |> LazyList.map
                             (fun (iOpt, s, b) ->
                                 View.Label(
                                     text = (if b then s else s + " "),
                                     fontAttributes = (if iOpt = model.CurrentMoveIndex then FontAttributes.Bold else FontAttributes.None),
                                     fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                     horizontalTextAlignment = TextAlignment.Center,
                                     verticalTextAlignment = TextAlignment.Center
                                 )
                             )
                       |> LazyList.toList
    View.FlexLayout(
        justifyContent = FlexJustify.Center,
        wrap = FlexWrap.Wrap,
        children = flexChildren
    )

let nagivation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
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
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGameWithBoards.MovesWithBoards.Length - 1)
                         then Icons.chevronRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToNextMove)
            )
            View.Button(
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGameWithBoards.MovesWithBoards.Length - 1)
                         then Icons.chevronDoubleRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToLastPos)
            )
        ]
    )
