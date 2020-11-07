module BlindfoldChessTraining.UIElems.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessMechanics
open FSharpx.Collections

let chessboard (model: Model.Model): ViewElement =
    let position = match model.CurrentMoveIndex with
                   | None ->
                        model.CurrentGame.InitialPosition
                   | Some i ->
                        model.CurrentGame.Moves
                        |> LazyList.ofArray
                        |> LazyList.take (i + 1)
                        |> LazyList.fold
                                (fun acc x -> Logic.Position.positionAfterMove x acc)
                                model.CurrentGame.InitialPosition
    UIElems.Board.grid model.ConfigOptions.AreSymbolsEnabled position.Board

let notation (model: Model.Model): ViewElement =
    let movesWithIndicators = model.CurrentGame.Moves
                              |> LazyList.ofArray
                              |> Notation.Emitter.moveTextsWithNumberIndicators
                                          model.ConfigOptions.AreSymbolsEnabled
                                          model.CurrentGame.InitialPosition.IsWhiteToMove
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
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGame.Moves.Length - 1)
                         then Icons.chevronRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToNextMove)
            )
            View.Button(
                image = (if model.CurrentMoveIndex <> Some (model.CurrentGame.Moves.Length - 1)
                         then Icons.chevronDoubleRight else Icons.empty),
                command = (fun () -> dispatch Msg.GoToLastPos)
            )
        ]
    )
