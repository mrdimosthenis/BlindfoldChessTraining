module BlindfoldChessTraining.Page.Options

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics.Notation

let separator(): ViewElement =
    View.BoxView(
        height = 1.0,
        color = Color.Accent
    )

let firstMove: Position.Move =
    { Piece = Board.Pawn
      FromCoords = (1, 4)
      ToCoords = (3, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let secondMove: Position.Move =
    { Piece = Board.Pawn
      FromCoords = (6, 4)
      ToCoords = (4, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let thirdMove: Position.Move =
    { Piece = Board.Knight
      FromCoords = (0, 6)
      ToCoords = (2, 5)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let forthMove: Position.Move =
    { Piece = Board.Knight
      FromCoords = (7, 1)
      ToCoords = (5, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let pieceNotationExample (areFigures: bool): string =
    [| firstMove
       secondMove
       thirdMove
       forthMove |]
    |> Seq.ofArray
    |> Emitter.multipleMovesText areFigures true

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
                View.StackLayout(
                    orientation = StackOrientation.Horizontal,
                    horizontalOptions = LayoutOptions.Center,
                    verticalOptions = LayoutOptions.Start,
                    children = [
                        View.Label(
                            text = "Options",
                            fontSize = FontSize.fromValue (Constants.titleSizeRatio * model.ConfigOptions.FontSize),
                            fontAttributes = FontAttributes.Bold,
                            horizontalOptions = LayoutOptions.Center,
                            verticalOptions = LayoutOptions.Center
                        )
                        View.Image(source = Icons.options)
                    ]
                )
                separator()
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
                            separator()
                            View.StackLayout(
                                orientation = StackOrientation.Horizontal,
                                horizontalOptions = LayoutOptions.Center,
                                children = [
                                    View.Label(
                                        text = "Piece Symbol:",
                                        fontSize = FontSize.fromValue model.ConfigOptions.FontSize
                                    )
                                    View.CheckBox(
                                        isChecked = model.ConfigOptions.AreSymbolsEnabled,
                                        checkedChanged = (fun args -> model.ConfigOptions.AreSymbolsEnabled |> not |> Msg.SelectPieceSymbolConfig |> dispatch)
                                    )
                                ]
                            )
                            View.Label(
                                text = (model.ConfigOptions.AreSymbolsEnabled |> pieceNotationExample |>  sprintf "Notation Example: %s"),
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            separator()
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
                            separator()
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
                            separator()
                            View.Label(
                                text = "Speech Accent:",
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            accentPicker model dispatch
                            View.Button(
                                text = "Speak",
                                horizontalOptions = LayoutOptions.Center,
                                command = (fun () -> Speech.speak model "blindfold chess training"),
                                image = Icons.speaker
                            )
                            View.Label(
                                text = "Please make sure that the accent is actually supported by the device. By pressing the button above, you should hear an example of the accent.",
                                fontSize = FontSize.fromValue model.ConfigOptions.FontSize,
                                horizontalTextAlignment = TextAlignment.Center
                            )
                            separator()
                            View.StackLayout(
                                orientation = StackOrientation.Horizontal,
                                horizontalOptions = LayoutOptions.Center,
                                verticalOptions = LayoutOptions.End,
                                children = [
                                    View.Button(
                                        text = "Reset ",
                                        horizontalOptions = LayoutOptions.Start,
                                        command = (fun () -> dispatch Msg.ResetConfigs),
                                        image = Icons.chip
                                    )
                                    View.Button(
                                        text = "Back",
                                        horizontalOptions = LayoutOptions.End,
                                        command = (fun () -> dispatch (Msg.SelectPage Model.HomePage)),
                                        image = Icons.home
                                    )
                                ]
                            )
                        ]
                    )
                )
            ]
        )
    )
