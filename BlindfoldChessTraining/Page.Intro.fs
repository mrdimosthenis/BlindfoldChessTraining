module BlindfoldChessTraining.Page.Intro

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open BlindfoldChessTraining

open System
open Xamarin.Essentials

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(
            verticalOptions = LayoutOptions.Center,
            horizontalOptions = LayoutOptions.Center,
            children = [
                View.Label(
                    text = "Created with",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.End
                )
                View.ImageButton(
                    source = Resources.image "logos.fabulous",
                    backgroundColor = Color.Transparent,
                    command = (fun () -> new Uri("https://fsprojects.github.io/Fabulous/") |> Launcher.OpenAsync |> Async.AwaitTask|> Async.StartImmediate)
                )
                View.Label(
                    text = "F# Functional App Development, using declarative dynamic UI",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.Start
                )
                View.ActivityIndicator(isRunning = true)
                View.Label(
                    text = "Wait until the app is ready to start",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.End
                )
            ]
        )
    )
