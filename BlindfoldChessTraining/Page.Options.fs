module BlindfoldChessTraining.Page.Options

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems

open BlindfoldChessMechanics.Logic

let accentPicker (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let accentTitle = "Default"
    let accentItems = model.Locales |> Speech.localeNames |> Seq.toList
    let accentF = (fun (i, _) -> dispatch (Msg.SelectLocaleConfig i))
    match model.ConfigOptions.SelectedLocale with
    | Some i when i < Seq.length model.Locales ->
         View.Picker(
             title = accentTitle,
             selectedIndex = i,
             items = accentItems,
             selectedIndexChanged = accentF,
             horizontalTextAlignment = TextAlignment.Center
         )
    | _ ->
        View.Picker(
            title = accentTitle,
            items = accentItems,
            selectedIndexChanged = accentF,
            horizontalTextAlignment = TextAlignment.Center
        )

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement = 
    View.ContentPage(
        content = View.StackLayout(
            horizontalOptions = LayoutOptions.Center,
            verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(
                    text = "Options",
                    fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                    fontAttributes = FontAttributes.Bold,
                    horizontalOptions = LayoutOptions.Center
                )
                View.ScrollView(
                    content = View.StackLayout(
                        horizontalOptions = LayoutOptions.Center,
                        verticalOptions = LayoutOptions.Center,
                        children = [
                            View.StackLayout(
                                orientation = StackOrientation.Horizontal,
                                horizontalOptions = LayoutOptions.Center,
                                children = [
                                    View.Label(
                                        text = "Board Coordinates:",
                                        fontSize = FontSize.fromValue model.ConfigOptions.FontSize
                                    )
                                    View.CheckBox(
                                        isChecked = model.ConfigOptions.AreCoordsEnabled,
                                        checkedChanged = (fun args -> model.ConfigOptions.AreCoordsEnabled |> not |> Msg.SelectCoordsConfig |> dispatch)
                                    )
                                ]
                            )

                            View.StackLayout(
                                orientation = StackOrientation.Horizontal,
                                horizontalOptions = LayoutOptions.Center,
                                children = [
                                    View.Label(
                                        text = "Piece Symbol Notation:",
                                        fontSize = FontSize.fromValue model.ConfigOptions.FontSize
                                    )
                                    View.CheckBox(
                                        isChecked = model.ConfigOptions.AreSymbolsEnabled,
                                        checkedChanged = (fun args -> model.ConfigOptions.AreSymbolsEnabled |> not |> Msg.SelectPieceSymbolConfig |> dispatch)
                                    )
                                ]
                            )

                            View.Label(
                                text = sprintf "Font Size: %.0f" model.ConfigOptions.FontSize,
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            View.Slider(
                                minimumMaximum = (1.0, 100.0),
                                valueChanged = (fun args -> args.NewValue |> Msg.SelectFontSizeConfig |> dispatch),
                                value = model.ConfigOptions.FontSize
                            )

                            View.Label(
                                text = sprintf "Speech Pitch: %.1f" model.ConfigOptions.SpeechPitch,
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            View.Slider(
                                minimumMaximum = (0.1, 2.0),
                                valueChanged = (fun args -> args.NewValue |> Msg.SelectPitchConfig |> dispatch),
                                value = model.ConfigOptions.SpeechPitch
                            )

                            View.Label(
                                text = "Speech Accent:",
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            accentPicker model dispatch
                            View.Button(text = "Speak", horizontalOptions = LayoutOptions.Center, command = fun () -> Speech.speak model "blindfold chess training")
                            View.Label(
                                text = "Please make sure that the accent is actually supported by the device. By pressing the button above, you should hear an example of the accent.",
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )

                            View.Button(text = "Reset ", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch Msg.ResetConfigs)
                            
                            View.Button(text = "Back", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.HomePage))
                        ]
                    )
                )
            ]
        )
    )
