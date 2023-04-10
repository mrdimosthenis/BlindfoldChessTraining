namespace BlindfoldChessTraining

open Xamarin.Forms
open Fabulous.XamarinForms

open FSharpx.Collections
open System
open Types

open type View

module App =

    // maybe setup
    if DB.doesTableExist () then
        ()
    else
        Preferences.clearPrefs ()
        DB.init ()

    // init model

    let initConfigOptions =
        { AreCoordsEnabled = Preferences.getAreCoordsEnabled ()
          AreSymbolsEnabled = Preferences.getAreSymbolsEnabled ()
          BoardSize = Preferences.getBoardSize ()
          FontSizeRatio = Preferences.getFontSizeRatio ()
          SelectedLocaleIndex = Preferences.getLocaleIndex ()
          SpeechPitch = Preferences.getSpeechPitch () }

    let initCurrentGame =
        DB.currentGame
            (Preferences.getAreSymbolsEnabled ())
            (Preferences.getCategoryId ())
            (Preferences.getLevel ())
            (Preferences.getIndexInLevel ())

    let init () =
        { SelectedPage = IntroPage
          Locales = LazyList.empty
          IsDisplayBoardOptionEnabled = Preferences.getIsDisplayBoardOptionEnabled ()
          ConfigOptions = initConfigOptions
          CurrentGame = initCurrentGame
          CurrentMoveIndex = None
          IsPuzzleSolved = false
          CurrentAnnouncementIndex = 0
          DidVolumeNoteClicked = Preferences.getDidVolumeNoteClicked ()
          LastVolumePressOrPanGestureMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds() }

    // update model

    let update msg model =
        match msg with
        | _ -> model

    // view model

    let view model =
        Application(
            ContentPage(
                "BlindfoldChessTraining",
                VStack() {
                    Label("Hello from Fabulous v2!")
                        .font(namedSize = NamedSize.Title)
                        .centerTextHorizontal ()

                    (VStack() {
                        Label($"Count is {model.CurrentAnnouncementIndex}").centerTextHorizontal ()

                        Button("Increment", Share)
                        Button("Decrement", Share)
                    })
                        .centerVertical (expand = true)
                }
            )
        )

    let program = Program.stateful init update view
