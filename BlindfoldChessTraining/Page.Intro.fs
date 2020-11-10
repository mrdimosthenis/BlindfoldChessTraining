module BlindfoldChessTraining.Page.Intro

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open BlindfoldChessTraining

open System
open Xamarin.Essentials

//let wait (dispatch: Msg.Msg -> unit) =
//    async {
//        do! Async.Sleep Constants.waitIntroMillis
//        dispatch Msg.SkipIntro
//    }

let view (dispatch: Msg.Msg -> unit): ViewElement =
    //wait(dispatch) |> Async.RunSynchronously
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
                    source = UIElems.Images.fabulous,
                    backgroundColor = Color.Transparent,
                    command = (fun () -> new Uri("https://fsprojects.github.io/Fabulous/") |> Launcher.OpenAsync |> Async.AwaitTask|> Async.StartImmediate)
                )
                View.Label(
                    text = "F# Functional App Development, using declarative dynamic UI",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.Start
                )
                View.ActivityIndicator(isRunning = true)
                View.Button(
                    text = "next",
                    command = fun () -> dispatch Msg.PrepareDB
                )
            ]
        )
    )
