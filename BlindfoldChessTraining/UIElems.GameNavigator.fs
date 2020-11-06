module BlindfoldChessTraining.UIElems.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessMechanics
open FSharpx.Collections

exception NoneCurrentGame

let notation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    match model.CurrentGame with
    | None ->
        raise NoneCurrentGame
    | Some currentGame ->
        let movesWithIndicators = currentGame.Moves
                                  |> LazyList.ofArray
                                  |> Notation.Emitter.moveTextsWithNumberIndicators
                                              model.ConfigOptions.AreSymbolsEnabled
                                              currentGame.InitialPosition.IsWhiteToMove
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
