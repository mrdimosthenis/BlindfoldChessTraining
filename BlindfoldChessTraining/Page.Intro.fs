module BlindfoldChessTraining.Page.Intro

open System

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Xamarin.Essentials

open BlindfoldChessTraining
open BlindfoldChessTraining.Template
open BlindfoldChessTraining.UIElems

let view (model: Model.Model) (_: Msg.Msg -> unit): ViewElement =
    let btn =
        fun () ->
            Uri("https://github.com/mrdimosthenis/mobile-apps")
            |> Launcher.OpenAsync
            |> Async.AwaitTask
            |> Async.StartImmediate
        |> Component.button "" Icons.list true
                                     
    View.ContentPage
        (backgroundColor = Constants.backgroundColor,
         content =
             View.StackLayout
                 (verticalOptions = LayoutOptions.Center,
                  horizontalOptions = LayoutOptions.Center,
                  children =
                      [ Component.label model false "...please wait a few seconds..."
                        btn
                        View.ActivityIndicator(isRunning = true) ]))
