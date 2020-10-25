module BlindfoldChessTraining.UIElems.Images

open Fabulous.XamarinForms
open Xamarin.Forms

let fromResourceName (name: string): Image.Value = 
    name
    |> sprintf "BlindfoldChessTraining.resources.images.%s.png"
    |> ImageSource.FromResource
    |> Image.fromImageSource
