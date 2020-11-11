module BlindfoldChessTraining.Template.Component

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining

let separator(): ViewElement =
    View.BoxView(
        height = 1.0,
        color = Color.Accent
    )

let button (title: string) (icon: Image.Value) (isMenuItem: bool) (f: unit -> unit): ViewElement =
    let (horizOpts, attributes) =
            if isMenuItem then (LayoutOptions.FillAndExpand, FontAttributes.Bold)
            else (LayoutOptions.Center, FontAttributes.None)
    View.Button(
        text = title,
        textTransform = TextTransform.None,
        fontAttributes = attributes,
        horizontalOptions = horizOpts,
        command = f,
        image = icon
    )

let label (model: Model.Model) (isLarge: bool) (title: string): ViewElement =
    let (multiplier, attributes) =
            if isLarge then (Constants.titleSizeRatio, FontAttributes.Bold)
            else (1.0, FontAttributes.None)
    View.Label(
        text = title,
        fontSize = (multiplier |> (*) model.ConfigOptions.FontSize |> FontSize.fromValue),
        fontAttributes = attributes,
        horizontalTextAlignment = TextAlignment.Center,
        verticalTextAlignment = TextAlignment.Center,
        horizontalOptions = LayoutOptions.Center,
        verticalOptions = LayoutOptions.Center
    )
