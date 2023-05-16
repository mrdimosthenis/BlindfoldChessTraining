module BlindfoldChessTraining.UIElems.Components

open BlindfoldChessTraining
open Fabulous.Maui
open Microsoft.Maui.Controls
open type Fabulous.Maui.View

let brush = Brush.DarkBlue

let btnIcon (imPath: string) msg =
    Button("", msg).image(imPath).background brush

let btnIconText (imPath: string) text msg =
    Button(text, msg).image(imPath).background brush

let label fontSizeRatio str =
    Label(str)
        .centerText()
        .font(size = fontSizeRatio * Constants.fontSize)
        .padding(10., 0., 10., 0.)
        .center ()
