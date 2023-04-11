namespace BlindfoldChessTraining

open Xamarin.Forms
open Fabulous.XamarinForms

open Fabulous
open FSharpx.Collections
open Xamarin.Essentials
open System
open Types

open type View

module App =

    // maybe setup
    if DB.doesTableExist () then
        ()
    else
        Preferences.reset ()
        DB.init ()

    // init model

    let initConfigOptions () =
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

    let initModel =
        { SelectedPage = IntroPage
          Locales = LazyList.empty
          IsDisplayBoardOptionEnabled = Preferences.getIsDisplayBoardOptionEnabled ()
          ConfigOptions = initConfigOptions ()
          CurrentGame = initCurrentGame
          CurrentMoveIndex = None
          IsPuzzleSolved = false
          CurrentAnnouncementIndex = 0
          DidVolumeNoteClicked = Preferences.getDidVolumeNoteClicked ()
          LastVolumePressOrPanGestureMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds() }

    let init () = initModel, Cmd.none

    // update model

    let update msg model =
        match msg with
        | LocalesLoaded v ->
            let cmd =
                async {
                    do! Async.Sleep Constants.introWaitMillis
                    return SelectPage HomePage
                }
                |> Cmd.ofAsyncMsg

            { model with Locales = v }, cmd

        | SelectPage v ->
            let newCategoryId =
                match v with
                | EndgamePuzzlesPage -> 0
                | OpeningPuzzlesPage -> 1
                | _ -> model.CurrentGame.CategoryId

            let newModel =
                { model with
                    SelectedPage = v
                    CurrentMoveIndex = None
                    IsPuzzleSolved = false
                    CurrentAnnouncementIndex = 0
                    CurrentGame =
                        DB.currentGame
                            model.ConfigOptions.AreSymbolsEnabled
                            newCategoryId
                            model.CurrentGame.Level
                            model.CurrentGame.IndexInLevel }

            newModel, Cmd.none

        | GoToMsg goToTarget ->
            let newLevel, newIndexInLevel =
                match goToTarget with
                | NextLevel ->
                    let level =
                        if model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2) then
                            0
                        else
                            model.CurrentGame.Level + 1

                    level, model.CurrentGame.IndexInLevel
                | PrevLevel ->
                    let level =
                        if model.CurrentGame.Level < 1 then
                            Constants.numOfLevelsPerCategory - 1
                        else
                            model.CurrentGame.Level - 1

                    level, model.CurrentGame.IndexInLevel
                | NextPuzzle ->
                    let doesLevelChange =
                        model.CurrentGame.IndexInLevel > (Constants.numOfPuzzlesPerLevel - 2)

                    let level =
                        match doesLevelChange, model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2) with
                        | false, _ -> model.CurrentGame.Level
                        | true, false -> model.CurrentGame.Level + 1
                        | true, true -> 0

                    let indexInLevel =
                        if doesLevelChange then
                            0
                        else
                            model.CurrentGame.IndexInLevel + 1

                    level, indexInLevel
                | PrevPuzzle ->
                    let doesLevelChange = model.CurrentGame.IndexInLevel < 1

                    let level =
                        match doesLevelChange, model.CurrentGame.Level < 1 with
                        | false, _ -> model.CurrentGame.Level
                        | true, false -> model.CurrentGame.Level - 1
                        | true, true -> Constants.numOfLevelsPerCategory - 1

                    let indexInLevel =
                        if doesLevelChange then
                            Constants.numOfPuzzlesPerLevel - 1
                        else
                            model.CurrentGame.IndexInLevel - 1

                    level, indexInLevel
                | _ -> model.CurrentGame.Level, model.CurrentGame.IndexInLevel

            let newCurrentMoveIndex =
                match goToTarget with
                | NextMove ->
                    match model.CurrentMoveIndex with
                    | None -> Some 0
                    | Some i when i = model.CurrentGame.Boards.Length - 1 -> model.CurrentMoveIndex
                    | Some i -> Some(i + 1)
                | PrevMove ->
                    match model.CurrentMoveIndex with
                    | None -> None
                    | Some 0 -> None
                    | Some i -> Some(i - 1)
                | LastPos -> Some(model.CurrentGame.Boards.Length - 1)
                | _ -> None

            let newCurrentGame =
                match goToTarget with
                | NextLevel
                | PrevLevel
                | NextPuzzle
                | PrevPuzzle ->
                    DB.currentGame
                        model.ConfigOptions.AreSymbolsEnabled
                        model.CurrentGame.CategoryId
                        newLevel
                        newIndexInLevel
                | _ -> model.CurrentGame

            let newModel =
                { model with
                    CurrentMoveIndex = newCurrentMoveIndex
                    CurrentGame = newCurrentGame }

            newModel, Cmd.none

        | Speak v ->
            let cmd =
                async {
                    do!
                        Speech.speak
                            model.ConfigOptions.SpeechPitch
                            model.Locales
                            model.ConfigOptions.SelectedLocaleIndex
                            v

                    return None
                }
                |> Cmd.ofAsyncMsgOption

            model, cmd

        | VolumeNoteClicked ->
            Preferences.setBool Preferences.didVolumeNoteClickedKey true

            { model with
                DidVolumeNoteClicked = true },
            Cmd.ofMsg VolumeUpPressed

        | ShowSolution ->
            let newCurrentMoveIndex =
                match model.CurrentGame.CategoryId with
                | 0 -> None
                | 1 -> Some(model.CurrentGame.Boards.Length - 2)
                | _ -> raise WrongCategoryId

            let newModel =
                { model with
                    CurrentMoveIndex = newCurrentMoveIndex
                    IsPuzzleSolved = true }

            newModel, Cmd.none

        | BackPressed ->
            if model.SelectedPage = HomePage then
                System.Diagnostics.Process.GetCurrentProcess().CloseMainWindow() |> ignore
            else
                ()

            { model with SelectedPage = HomePage }, Cmd.none

        | VolumeUpPressed
        | PanRightGesture ->
            let currentMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds()

            let isDebounceThresholdOver =
                currentMillis - model.LastVolumePressOrPanGestureMillis > Constants.volumePressOrPanGestureDebounceTimeout

            match isDebounceThresholdOver, model.SelectedPage with
            | true, OpeningPuzzlesPage
            | true, EndgamePuzzlesPage ->
                let cmd = model.CurrentGame.Announcements[0] |> Speak |> Cmd.ofMsg

                { model with
                    CurrentAnnouncementIndex = 1
                    LastVolumePressOrPanGestureMillis = currentMillis },
                cmd
            | _ -> model, Cmd.none

        | VolumeDownPressed
        | PanLeftGesture ->
            let currentMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds()

            let isDebounceThresholdOver =
                currentMillis - model.LastVolumePressOrPanGestureMillis > Constants.volumePressOrPanGestureDebounceTimeout

            match isDebounceThresholdOver, model.SelectedPage with
            | true, OpeningPuzzlesPage
            | true, EndgamePuzzlesPage ->
                match model.CurrentAnnouncementIndex with
                | v when v = model.CurrentGame.Announcements.Length ->
                    let newModel =
                        { model with
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let firstCmd = "next puzzle" |> Speak |> Cmd.ofMsg
                    let secondCmd = Cmd.ofMsg (GoToMsg NextPuzzle)
                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    newModel, cmd
                | v when v = model.CurrentGame.Announcements.Length - 1 ->
                    let newModel =
                        { model with
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let firstCmd = Cmd.ofMsg ShowSolution

                    let secondCmd = model.CurrentGame.Announcements[v] |> Speak |> Cmd.ofMsg

                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    newModel, cmd
                | v ->
                    let newModel =
                        { model with
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let cmd = model.CurrentGame.Announcements[v] |> Speak |> Cmd.ofMsg

                    newModel, cmd
            | _ -> model, Cmd.none

        | SelectDisplayBoardOption v ->
            Preferences.setBool Preferences.isDisplayBoardOptionEnabledKey v

            { model with
                IsDisplayBoardOptionEnabled = v },
            Cmd.none

        | SetConfig configMsg ->
            let newConfigOptions =

                match configMsg with

                | SwitchAreCoordsEnabled ->
                    let v = not model.ConfigOptions.AreCoordsEnabled
                    Preferences.setBool Preferences.areCoordsEnabledKey v

                    { model.ConfigOptions with
                        AreCoordsEnabled = v }

                | SwitchAreSymbolsEnabled ->
                    let v = not model.ConfigOptions.AreSymbolsEnabled
                    Preferences.setBool Preferences.areSymbolsEnabledKey v

                    { model.ConfigOptions with
                        AreSymbolsEnabled = v }

                | SetBoardSize v ->
                    Preferences.setFloat Preferences.boardSizeKey v

                    { model.ConfigOptions with
                        BoardSize = v }

                | SetFontSizeRatio v ->
                    Preferences.setFloat Preferences.fontSizeRatioKey v

                    { model.ConfigOptions with
                        FontSizeRatio = v }
                | SetSpeechPitch v ->
                    Preferences.setFloat32 Preferences.speechPitchKey v

                    { model.ConfigOptions with
                        SpeechPitch = v }
                | SetSelectedLocaleIndex v ->
                    Preferences.setInt Preferences.selectedLocaleIndexKey v

                    { model.ConfigOptions with
                        SelectedLocaleIndex = Some v }
                | Reset ->
                    Preferences.reset ()
                    initConfigOptions ()

            { model with
                ConfigOptions = newConfigOptions },
            Cmd.none

        | UrlClick externalUrl ->
            match externalUrl with
            | GitHub -> Constants.gitHubUrl
            | LinkedIn -> Constants.linkedInUrl
            | AppStore -> Constants.appStoreUrl
            | PrivacyPolicy -> Constants.privacyPolicyUrl
            |> Uri
            |> Launcher.OpenAsync
            |> Async.AwaitTask
            |> Async.StartImmediate

            model, Cmd.none

        | Share ->
            Constants.appStoreUrl
            |> Share.RequestAsync
            |> Async.AwaitTask
            |> Async.StartImmediate

            model, Cmd.none

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

    let program = Program.statefulWithCmd init update view
