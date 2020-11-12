module BlindfoldChessTraining.Page.Intro

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open BlindfoldChessTraining

open System
open Xamarin.Essentials

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        backgroundColor = Constants.backgroundColor,
        content = View.StackLayout(
            verticalOptions = LayoutOptions.Center,
            horizontalOptions = LayoutOptions.Center,
            children = [
                View.Label(
                    text = "...created with...",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.End
                )
                View.ImageButton(
                    source = UIElems.Images.fabulous,
                    backgroundColor = Color.Transparent,
                    command = (fun () -> new Uri("https://fsprojects.github.io/Fabulous/") |> Launcher.OpenAsync |> Async.AwaitTask|> Async.StartImmediate)
                )
                View.ActivityIndicator(isRunning = true)
            ]
        )
    )
