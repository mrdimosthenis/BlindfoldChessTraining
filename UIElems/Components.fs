module BlindfoldChessTraining.UIElems.Components

open BlindfoldChessTraining
open Fabulous.Maui
open type Fabulous.Maui.View

let btnIcon (imPath: string) msg = Button("", msg).image imPath

let label fontSizeRatio str =
    Label(str)
        .centerText()
        .font(size = fontSizeRatio * Constants.fontSize)
        .padding(10., 0., 10., 0.)
        .center ()
