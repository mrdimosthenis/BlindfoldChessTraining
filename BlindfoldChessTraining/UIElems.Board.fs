module BlindfoldChessTraining.UIElems.Board

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

let pngImgElem (imgName: string): ViewElement =
    let pngImgSrc = imgName
                    |> sprintf "BlindfoldChessTraining.resources.images.board.%s.png"
                    |> ImageSource.FromResource
                    |> Image.fromImageSource
    View.Image(source = pngImgSrc)

let rowElem (pngNames: string list): ViewElement =
    View.StackLayout(
        orientation = StackOrientation.Horizontal,
        horizontalOptions = LayoutOptions.Center,
        spacing = 0.0,
        children = List.map pngImgElem pngNames
    )

let boardElem (pngNames: string list list): ViewElement =
    View.StackLayout(
        verticalOptions = LayoutOptions.Center,
        spacing = 0.0,
        children = List.map rowElem pngNames
    )

let emptyBoardElem: ViewElement =
    boardElem [ [ "t_l"; "t";  "t";  "t";  "t";  "t";  "t";  "t";  "t";  "t_r" ]
                [ "8";   "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "r" ]
                [ "7";   "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "r" ]
                [ "6";   "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "r" ]
                [ "5";   "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "r" ]
                [ "4";   "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "r" ]
                [ "3";   "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "r" ]
                [ "2";   "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "r" ]
                [ "1";   "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "bl"; "wh"; "r" ]
                [ "b_l"; "a";  "b";  "c";  "d";  "e";  "f";  "g";  "h";  "b_r" ] ]
