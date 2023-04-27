module BlindfoldChessTraining.Pages.Options

open BlindfoldChessMechanics
open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open FSharpx.Collections
open type Fabulous.Maui.View

let firstMove: Logic.Position.Move =
    { Piece = Logic.Board.Pawn
      FromCoords = (1, 4)
      ToCoords = (3, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = [||] }

let secondMove: Logic.Position.Move =
    { Piece = Logic.Board.Pawn
      FromCoords = (6, 4)
      ToCoords = (4, 4)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = [||] }

let thirdMove: Logic.Position.Move =
    { Piece = Logic.Board.Bishop
      FromCoords = (0, 5)
      ToCoords = (3, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = [||] }

let forthMove: Logic.Position.Move =
    { Piece = Logic.Board.Knight
      FromCoords = (7, 1)
      ToCoords = (5, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = [||] }

let pieceNotationExample areSymbolsEnabled =
    [ firstMove; secondMove; thirdMove; forthMove ]
    |> LazyList.ofList
    |> Notation.Emitter.multipleMovesText areSymbolsEnabled true

let accentPicker locales localeIndex =
    let accentItems = locales |> Speech.localeNames |> LazyList.toList
    
    let i = Speech.safeLocaleIndex locales localeIndex

    Picker(accentItems, i, (fun v -> SetConfig(SetLocaleIndex v)))
        .centerVertical ()

let view model =

    let { Locales = locales
          ConfigOptions = configOptions } =
        model

    let { AreCoordsEnabled = areCoordsEnabled
          AreSymbolsEnabled = areSymbolsEnabled
          FontSizeRatio = fontSizeRatio
          BoardSizeRatio = boardSizeRatio
          LocaleIndex = localeIndex
          SpeechPitch = speechPitch } =
        configOptions


    let innerElems =
        [ Logic.Board.init
          |> UIElems.Board.boardGrid areCoordsEnabled boardSizeRatio
          |> Grd

          HorizSt(
              (HStack() {
                  UIElems.Components.label fontSizeRatio "Board Coordinates:"
                  CheckBox(areCoordsEnabled, (fun _ -> SetConfig SwitchAreCoordsEnabled))
              })
                  .centerHorizontal ()
          )

          $"Board Size: %.2f{boardSizeRatio}"
          |> UIElems.Components.label fontSizeRatio
          |> Lbl

          Sld(Slider(0.3, 1.0, boardSizeRatio, (fun v -> SetConfig(SetBoardSizeRatio v))))

          HorizSt(
              (HStack() {
                  UIElems.Components.label fontSizeRatio "Piece Symbol Notation:"
                  CheckBox(areSymbolsEnabled, (fun _ -> SetConfig SwitchAreSymbolsEnabled))
              })
                  .centerHorizontal ()
          )

          HorizSt(
              (HStack() {
                  UIElems.Components.label fontSizeRatio "Example:"

                  areSymbolsEnabled
                  |> pieceNotationExample
                  |> UIElems.Components.label fontSizeRatio
              })
                  .centerHorizontal ()
          )

          $"Font Size: %.2f{fontSizeRatio}"
          |> UIElems.Components.label fontSizeRatio
          |> Lbl

          Sld(Slider(0.5, 2.5, fontSizeRatio, (fun v -> SetConfig(SetFontSizeRatio v))))

          $"Speech Pitch: %.1f{speechPitch}"
          |> UIElems.Components.label fontSizeRatio
          |> Lbl

          Sld(Slider(0.3, 2.0, float speechPitch, (fun v -> v |> float32 |> SetSpeechPitch |> SetConfig)))

          HorizSt(
              (HStack() {
                  UIElems.Components.label fontSizeRatio "Speech Accent:"
                  accentPicker locales localeIndex
              })
                  .centerHorizontal ()
          )

          "Blindfold Chess Training" |> Speak |> UIElems.Icons.speaker "Speak" |> Btn

          "Please make sure that the accent is actually supported by the device. By pressing the button above, you should hear an example of the accent."
          |> UIElems.Components.label fontSizeRatio
          |> Lbl

          Reset |> SetConfig |> UIElems.Icons.chip "Reset to Default" |> Btn ]

    UIElems.Page.template model "Options" UIElems.Icons.optionsColored innerElems
