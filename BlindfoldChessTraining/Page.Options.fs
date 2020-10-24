module BlindfoldChessTraining.Page.Options

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms
open BlindfoldChessTraining

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(
            horizontalOptions = LayoutOptions.Center,
            verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(text = "Options", fontAttributes = FontAttributes.Bold, horizontalOptions = LayoutOptions.Center)
                View.StackLayout(
                    orientation = StackOrientation.Horizontal,
                    horizontalOptions = LayoutOptions.Center,
                    children = [
                        View.Label(text = "Display Board Coordinates:")
                        View.CheckBox(
                            isChecked = model.ConfigOptions.AreCoordsEnabled,
                            checkedChanged = (fun args -> model.ConfigOptions.AreCoordsEnabled |> not |> Msg.SelectCoordsConfig |> dispatch)
                        )
                    ]
                )

                View.Label(
                    text = sprintf "Speech Pitch: %.1f" model.ConfigOptions.SpeechPitch,
                    horizontalTextAlignment = TextAlignment.Center
                )
                View.Slider(
                    minimumMaximum = (0.1, 2.0),
                    //horizontalOptions = LayoutOptions.FillAndExpand,
                    valueChanged = (fun args -> args.NewValue |> Msg.SelectPitchConfig |> dispatch),
                    value = model.ConfigOptions.SpeechPitch
                )
                
                View.Button(text = "Back", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.HomePage))
            ]
        )
    )
