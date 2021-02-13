module BlindfoldChessTraining.Update

open System

open FSharpx.Collections

open Fabulous
open Xamarin.Essentials

open BlindfoldChessMechanics

// analytics

let modelTrackingSubMap (model: Model.Model): Map<string, string> =
    [ ("selectedPage", string model.SelectedPage)
      ("boardSize", string model.ConfigOptions.BoardSize)
      ("fontSize", string model.ConfigOptions.FontSize)
      ("selectedLocale",
       match model.ConfigOptions.SelectedLocale with
       | Some i when i < LazyList.length model.Locales -> model.Locales |> Speech.localeNames |> Seq.item i
       | _ -> "Default")
      ("speechPitch", string model.ConfigOptions.SpeechPitch)
      ("userId", model.UserId)
      ("sessionId", model.SessionId) ]
    |> Map.ofList

let maybeTrackEvent (msg: Msg.Msg) (model: Model.Model): unit =
    if model.AreAnalyticsEnabled then
        match msg with
        | Msg.GoToNextLevel
        | Msg.GoToPrevLevel
        | Msg.GoToNextPuzzle
        | Msg.GoToPrevPuzzle
        | Msg.VolumeNoteClicked
        | Msg.ShowSolution
        | Msg.SelectDisplayBoardOption _
        | Msg.SelectCoordsConfig _
        | Msg.SelectPieceSymbolConfig _
        | Msg.ResetConfigs
        | Msg.UrlClick _
        | Msg.UrlShare _
        | Msg.SwitchAnalytics ->
            let modelSubMap = modelTrackingSubMap model

            let eventJsonString =
                Newtonsoft.Json.JsonConvert.SerializeObject msg

            Tracking.track modelSubMap eventJsonString
        | _ -> ()
    else
        ()

// update related functions

let cmdInit (): Cmd<Msg.Msg> =
    async {
        let! locales = Speech.loadLocales ()
        let localesMsg = Msg.LocalesLoaded locales
        return localesMsg
    }
    |> Cmd.ofAsyncMsg

// workaround for problematic rendering after option change
let rerenderOptionsPage: Cmd<Msg.Msg> =
    let cmd1 =
        Model.HomePage |> Msg.SelectPage |> Cmd.ofMsg

    let cmd2 =
        Model.OptionsPage |> Msg.SelectPage |> Cmd.ofMsg

    Cmd.batch [ cmd1; cmd2 ]

let getNewGameFromDBAndModel (model: Model.Model) (categoryId: int, level: int, indexInLevel: int): Model.Model =
    let newGameJsonStr =
        DB.getGameJsonStr (categoryId, level, indexInLevel)

    let newCurrentGame =
        newGameJsonStr
        |> Notation.Parser.jsonOfGame
        |> Model.mechanicToCurrentGame model.ConfigOptions.AreSymbolsEnabled model.SelectedPage

    match model.SelectedPage with
    | Model.OpeningPuzzlesPage ->
        Preferences.setString Preferences.openingJsonStrKey newGameJsonStr

        { model with
              CurrentGame = newCurrentGame
              CurrentMoveIndex = None
              OpeningJsonStr = newGameJsonStr
              IsPuzzleSolved = false
              CurrentAnnouncementIndex = 0 }
    | _ ->
        Preferences.setString Preferences.endgameJsonStrKey newGameJsonStr

        { model with
              CurrentGame = newCurrentGame
              CurrentMoveIndex = None
              EndgameJsonStr = newGameJsonStr
              IsPuzzleSolved = false
              CurrentAnnouncementIndex = 0 }

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    maybeTrackEvent msg model

    match msg with
    | Msg.LocalesLoaded v ->
        let cmd =
            async {
                do! Async.Sleep Constants.introWaitMillis
                return Msg.SelectPage Model.HomePage
            }
            |> Cmd.ofAsyncMsg

        { model with Locales = v }, cmd

    | Msg.SelectPage v ->
        let newCurrentGame =
            match v with
            | Model.OpeningPuzzlesPage ->
                model.OpeningJsonStr
                |> Notation.Parser.jsonOfGame
                |> Model.mechanicToCurrentGame model.ConfigOptions.AreSymbolsEnabled v
            | Model.EndgamePuzzlesPage ->
                model.EndgameJsonStr
                |> Notation.Parser.jsonOfGame
                |> Model.mechanicToCurrentGame model.ConfigOptions.AreSymbolsEnabled v
            | _ -> model.CurrentGame

        let newModel =
            { model with
                  SelectedPage = v
                  CurrentGame = newCurrentGame
                  CurrentMoveIndex = None
                  IsPuzzleSolved = false
                  CurrentAnnouncementIndex = 0 }

        newModel, Cmd.none

    | Msg.GoToPrevLevel ->
        let categoryId = model.CurrentGame.CategoryId

        let level =
            if model.CurrentGame.Level < 1 then Constants.numOfLevelsPerCategory - 1 else model.CurrentGame.Level - 1

        let indexInLevel = model.CurrentGame.IndexInLevel

        let newModel =
            getNewGameFromDBAndModel model (categoryId, level, indexInLevel)

        newModel, Cmd.none
    | Msg.GoToNextLevel ->
        let categoryId = model.CurrentGame.CategoryId

        let level =
            if model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2)
            then 0
            else model.CurrentGame.Level + 1

        let indexInLevel = model.CurrentGame.IndexInLevel

        let newModel =
            getNewGameFromDBAndModel model (categoryId, level, indexInLevel)

        newModel, Cmd.none
    | Msg.GoToPrevPuzzle ->
        let doesLevelChange = model.CurrentGame.IndexInLevel < 1
        let categoryId = model.CurrentGame.CategoryId

        let level =
            match (doesLevelChange, model.CurrentGame.Level < 1) with
            | (false, _) -> model.CurrentGame.Level
            | (true, false) -> model.CurrentGame.Level - 1
            | (true, true) -> Constants.numOfLevelsPerCategory - 1

        let indexInLevel =
            if doesLevelChange then Constants.numOfPuzzlesPerLevel - 1 else model.CurrentGame.IndexInLevel - 1

        let newModel =
            getNewGameFromDBAndModel model (categoryId, level, indexInLevel)

        newModel, Cmd.none
    | Msg.GoToNextPuzzle ->
        let doesLevelChange =
            model.CurrentGame.IndexInLevel > (Constants.numOfPuzzlesPerLevel - 2)

        let categoryId = model.CurrentGame.CategoryId

        let level =
            match (doesLevelChange, model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2)) with
            | (false, _) -> model.CurrentGame.Level
            | (true, false) -> model.CurrentGame.Level + 1
            | (true, true) -> 0

        let indexInLevel =
            if doesLevelChange then 0 else model.CurrentGame.IndexInLevel + 1

        let newModel =
            getNewGameFromDBAndModel model (categoryId, level, indexInLevel)

        newModel, Cmd.none

    | Msg.GoToNextMove ->
        let newCurrentMoveIndex =
            match model.CurrentMoveIndex with
            | None -> Some 0
            | Some i when i = model.CurrentGame.Boards.Length - 1 -> model.CurrentMoveIndex
            | Some i -> Some(i + 1)

        { model with
              CurrentMoveIndex = newCurrentMoveIndex },
        Cmd.none
    | Msg.GoToPrevMove ->
        let newCurrentMoveIndex =
            match model.CurrentMoveIndex with
            | None -> None
            | Some 0 -> None
            | Some i -> Some(i - 1)

        { model with
              CurrentMoveIndex = newCurrentMoveIndex },
        Cmd.none
    | Msg.GoToInitPos -> { model with CurrentMoveIndex = None }, Cmd.none
    | Msg.GoToLastPos ->
        let newCurrentMoveIndex =
            Some(model.CurrentGame.Boards.Length - 1)

        { model with
              CurrentMoveIndex = newCurrentMoveIndex },
        Cmd.none

    | Msg.Speak v ->
        let cmd =
            async {
                do! Speech.speak model.ConfigOptions.SpeechPitch model.Locales model.ConfigOptions.SelectedLocale v
                return None
            }
            |> Cmd.ofAsyncMsgOption

        model, cmd

    | Msg.VolumeNoteClicked ->
        Preferences.setBool Preferences.didVolumeNoteClickedKey true

        { model with
              DidVolumeNoteClicked = true },
        Cmd.ofMsg Msg.VolumeUpPressed

    | Msg.ShowSolution ->
        let newCurrentMoveIndex =
            match model.SelectedPage with
            | Model.OpeningPuzzlesPage -> Some(model.CurrentGame.Boards.Length - 2)
            | _ -> None

        let newModel =
            { model with
                  CurrentMoveIndex = newCurrentMoveIndex
                  IsPuzzleSolved = true }

        newModel, Cmd.none

    | Msg.BackPressed ->
        if model.SelectedPage = Model.HomePage then
            System
                .Diagnostics
                .Process
                .GetCurrentProcess()
                .CloseMainWindow()
            |> ignore
        else
            ()

        { model with
              SelectedPage = Model.HomePage },
        Cmd.none

    | Msg.VolumeUpPressed
    | Msg.PanRightGesture ->
        let currentMillis =
            DateTimeOffset.Now.ToUnixTimeMilliseconds()

        let isDebounceThresholdOver =
            currentMillis
            - model.LastVolumePressOrPanGestureMillis > Constants.volumePressOrPanGestureDebounceTimeout

        match (isDebounceThresholdOver, model.SelectedPage) with
        | (true, Model.OpeningPuzzlesPage)
        | (true, Model.EndgamePuzzlesPage) ->
            let cmd =
                model.CurrentGame.Announcements.[0]
                |> Msg.Speak
                |> Cmd.ofMsg

            { model with
                  CurrentAnnouncementIndex = 1
                  LastVolumePressOrPanGestureMillis = currentMillis },
            cmd
        | _ -> model, Cmd.none

    | Msg.VolumeDownPressed
    | Msg.PanLeftGesture ->
        let currentMillis =
            DateTimeOffset.Now.ToUnixTimeMilliseconds()

        let isDebounceThresholdOver =
            currentMillis
            - model.LastVolumePressOrPanGestureMillis > Constants.volumePressOrPanGestureDebounceTimeout

        match (isDebounceThresholdOver, model.SelectedPage) with
        | (true, Model.OpeningPuzzlesPage)
        | (true, Model.EndgamePuzzlesPage) ->
            match model.CurrentAnnouncementIndex with
            | v when v = model.CurrentGame.Announcements.Length ->
                let newModel =
                    { model with
                          LastVolumePressOrPanGestureMillis = currentMillis }

                let firstCmd = "next puzzle" |> Msg.Speak |> Cmd.ofMsg
                let secondCmd = Cmd.ofMsg Msg.GoToNextPuzzle
                let cmd = Cmd.batch [ firstCmd; secondCmd ]
                newModel, cmd
            | v when v = model.CurrentGame.Announcements.Length - 1 ->
                let newModel =
                    { model with
                          CurrentAnnouncementIndex = v + 1
                          LastVolumePressOrPanGestureMillis = currentMillis }

                let firstCmd = Cmd.ofMsg Msg.ShowSolution

                let secondCmd =
                    model.CurrentGame.Announcements.[v]
                    |> Msg.Speak
                    |> Cmd.ofMsg

                let cmd = Cmd.batch [ firstCmd; secondCmd ]
                newModel, cmd
            | v ->
                let newModel =
                    { model with
                          CurrentAnnouncementIndex = v + 1
                          LastVolumePressOrPanGestureMillis = currentMillis }

                let cmd =
                    model.CurrentGame.Announcements.[v]
                    |> Msg.Speak
                    |> Cmd.ofMsg

                newModel, cmd
        | _ -> model, Cmd.none

    | Msg.SelectDisplayBoardOption v ->
        Preferences.setBool Preferences.isDisplayBoardOptionEnabledKey v

        { model with
              IsDisplayBoardOptionEnabled = v },
        Cmd.none

    | Msg.SelectCoordsConfig v ->
        Preferences.setBool Preferences.areCoordsEnabledKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  AreCoordsEnabled = v }

        let cmd =
            model.SelectedPage |> Msg.SelectPage |> Cmd.ofMsg

        { model with
              ConfigOptions = newConfigOptions },
        cmd
    | Msg.SelectBoardSizeConfig v ->
        Preferences.setFloat Preferences.boardSizeKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  BoardSize = v }

        { model with
              ConfigOptions = newConfigOptions },
        rerenderOptionsPage
    | Msg.SelectPieceSymbolConfig v ->
        Preferences.setBool Preferences.areSymbolsEnabledKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  AreSymbolsEnabled = v }

        { model with
              ConfigOptions = newConfigOptions },
        Cmd.none
    | Msg.SelectFontSizeConfig v ->
        Preferences.setFloat Preferences.fontSizeKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  FontSize = v }

        { model with
              ConfigOptions = newConfigOptions },
        Cmd.none
    | Msg.SelectPitchConfig v ->
        Preferences.setFloat Preferences.speechPitchKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  SpeechPitch = v }

        { model with
              ConfigOptions = newConfigOptions },
        Cmd.none
    | Msg.SelectLocaleConfig v ->
        Preferences.setInt Preferences.selectedLocaleKey v

        let newConfigOptions =
            { model.ConfigOptions with
                  SelectedLocale = Some v }

        { model with
              ConfigOptions = newConfigOptions },
        Cmd.none
    | Msg.ResetConfigs ->
        Model.resetConfigOptions ()

        { model with
              ConfigOptions = Model.initConfigOptions () },
        rerenderOptionsPage

    | Msg.UrlClick externalUrl ->
        match externalUrl with
        | Msg.GitHub -> Constants.gitHubUrl
        | Msg.LinkedIn -> Constants.linkedInUrl
        | Msg.GooglePlay -> Constants.googlePlayUrl
        | Msg.AppleStore -> Constants.appleStoreUrl
        | Msg.PrivacyPolicy -> Constants.privacyPolicyUrl
        |> Uri
        |> Launcher.OpenAsync
        |> Async.AwaitTask
        |> Async.StartImmediate

        model, Cmd.none

    | Msg.UrlShare sharableUrl ->
        match sharableUrl with
        | Msg.AppOnGooglePlay -> Constants.googlePlayUrl
        | Msg.AppOnAppleStore -> Constants.appleStoreUrl
        |> Share.RequestAsync
        |> Async.AwaitTask
        |> Async.StartImmediate

        model, Cmd.none

    | Msg.SwitchAnalytics ->
        let areAnalyticsEnabled = not model.AreAnalyticsEnabled
        Preferences.setBool Preferences.areAnalyticsEnabledKey areAnalyticsEnabled

        if areAnalyticsEnabled then Tracking.initialize () else Tracking.stop ()

        { model with
              AreAnalyticsEnabled = areAnalyticsEnabled },
        Cmd.none
