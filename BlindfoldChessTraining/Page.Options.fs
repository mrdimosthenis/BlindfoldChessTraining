module BlindfoldChessTraining.Page.Options

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

open BlindfoldChessMechanics
open FSharpx.Collections

let firstMove: Logic.Position.Move =
    { Piece = Logic.Board.Pawn
      FromCoords = (1, 4)
      ToCoords = (3, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let secondMove: Logic.Position.Move =
    { Piece = Logic.Board.Pawn
      FromCoords = (6, 4)
      ToCoords = (4, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let thirdMove: Logic.Position.Move =
    { Piece = Logic.Board.Bishop
      FromCoords = (0, 5)
      ToCoords = (3, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let forthMove: Logic.Position.Move =
    { Piece = Logic.Board.Knight
      FromCoords = (7, 1)
      ToCoords = (5, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }

let pieceNotationExample (areFigures: bool): string =
    [ firstMove
      secondMove
      thirdMove
      forthMove ]
    |> LazyList.ofList
    |> Notation.Emitter.multipleMovesText areFigures true

let accentPicker (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let accentTitle = "Default"
    let accentItems = model.Locales |> Speech.localeNames |> LazyList.toList
    let accentF = (fun (i, _) -> dispatch (Msg.SelectLocaleConfig i))
    match model.ConfigOptions.SelectedLocale with
    | Some i when i < LazyList.length model.Locales ->
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
    let innerElems =
            [ Board.grid model.ConfigOptions.AreCoordsEnabled Logic.Board.init
              View.StackLayout(
                  orientation = StackOrientation.Horizontal,
                  horizontalOptions = LayoutOptions.Center,
                  children = [
                      Component.label model false "Board Coordinates:"
                      View.CheckBox(
                          isChecked = model.ConfigOptions.AreCoordsEnabled,
                          checkedChanged = (fun args -> model.ConfigOptions.AreCoordsEnabled |> not |> Msg.SelectCoordsConfig |> dispatch)
                      )
                  ]
              )
              Component.separator()
              View.StackLayout(
                  orientation = StackOrientation.Horizontal,
                  horizontalOptions = LayoutOptions.Center,
                  children = [
                      Component.label model false "Piece Symbol Notation:"
                      View.CheckBox(
                          isChecked = model.ConfigOptions.AreSymbolsEnabled,
                          checkedChanged = (fun args -> model.ConfigOptions.AreSymbolsEnabled |> not |> Msg.SelectPieceSymbolConfig |> dispatch)
                      )
                  ]
              )
              model.ConfigOptions.AreSymbolsEnabled |> pieceNotationExample |>  sprintf "Example: %s" |> Component.label model false
              Component.separator()
              model.ConfigOptions.FontSize |> sprintf "Font Size: %.0f" |> Component.label model false
              View.Slider(
                  minimumMaximum = (1.0, 100.0),
                  valueChanged = (fun args -> args.NewValue |> Msg.SelectFontSizeConfig |> dispatch),
                  value = model.ConfigOptions.FontSize
              )
              Component.separator()
              model.ConfigOptions.SpeechPitch |> sprintf "Speech Pitch: %.1f" |> Component.label model false
              View.Slider(
                  minimumMaximum = (0.1, 2.0),
                  valueChanged = (fun args -> args.NewValue |> Msg.SelectPitchConfig |> dispatch),
                  value = model.ConfigOptions.SpeechPitch
              )
              Component.separator()
              Component.label model false "Speech Accent:"
              accentPicker model dispatch
              Component.button "Speak" Icons.speaker false (fun () -> "Blindfold Chess Training" |> Msg.Speak |> dispatch)
              Component.label model false "Please make sure that the accent is actually supported by the device. By pressing the button above, you should hear an example of the accent."
              Component.separator()
              Component.button "Reset to Default" Icons.chip false (fun () -> dispatch Msg.ResetConfigs) ]
    Page.page model dispatch "Options" Icons.options innerElems
