module BlindfoldChessTraining.UIElems.Components

open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open Microsoft.Maui.Controls
open type Fabulous.Maui.View

let brush = Brush.DarkBlue

let btnIcon (imPath: string) msg =
    let btn = Button("", msg).image imPath
    match Constants.os with
    | Android -> btn
    | IOS -> btn.background brush

let btnIconText (imPath: string) text msg =
    let btn = Button(text, msg).image imPath
    match Constants.os with
    | Android -> btn
    | IOS -> btn.background brush

let label fontSizeRatio str =
    Label(str)
        .centerText()
        .font(size = fontSizeRatio * Constants.fontSize)
        .padding(10., 0., 10., 0.)
        .center ()
