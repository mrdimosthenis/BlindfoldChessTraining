module BlindfoldChessTraining.UIElems.GameNavigator

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessMechanics
open FSharpx.Collections

let notation (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let currentGame = model.CurrentGame.Value
    let flexChildren = currentGame.Moves
                       |> LazyList.ofArray
                       |> Notation.Emitter.moveTextsWithNumberIndicators
                                   model.ConfigOptions.AreSymbolsEnabled
                                   currentGame.InitialPosition.IsWhiteToMove
                       |> LazyList.map
                                   (fun (s, b) ->
                                       View.Label(
                                           text = (if b then s else s + " "),
                                           fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                           fontAttributes = (if b then FontAttributes.None else FontAttributes.Bold),
                                           horizontalTextAlignment = TextAlignment.Center,
                                           verticalTextAlignment = TextAlignment.Center
                                       )
                                   )
                       |> LazyList.toList
    View.FlexLayout(
        //horizontalOptions = LayoutOptions.Center,
        //alignItems = FlexAlignItems.Center,
        //alignContent = FlexAlignContent.Center,
        justifyContent = FlexJustify.Center,
        wrap = FlexWrap.Wrap,
        children = flexChildren
    )
