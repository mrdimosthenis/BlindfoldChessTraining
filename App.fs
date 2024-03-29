namespace BlindfoldChessTraining

open BlindfoldChessTraining.Types
open FSharpx.Collections
open Fabulous
open Fabulous.Maui
open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.ApplicationModel.DataTransfer
open System
open type Fabulous.Maui.View

module CodeReceivedService =
    let mutable Instance: IKeyCodeReceivedService = Unchecked.defaultof<_>

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
          BoardSizeRatio = Preferences.getBoardSizeRatio ()
          FontSizeRatio = Preferences.getFontSizeRatio ()
          LocaleIndex = Preferences.getLocaleIndex ()
          SpeechPitch = Preferences.getSpeechPitch () }

    let initSponsorDetails = Resources.sponsorDetails ()

    let initModel =
        { SponsorDetails = initSponsorDetails
          IsSponsorTime = initSponsorDetails.IsSome
          SelectedPage = HomePage
          Locales = LazyList.empty
          IsDisplayBoardEnabled = Preferences.getIsDisplayBoardEnabled ()
          ConfigOptions = initConfigOptions ()
          CurrentGame = DB.currentGame false 0 0 0
          CurrentMoveIndex = None
          IsPuzzleSolved = false
          CurrentAnnouncementIndex = 0
          DidSpeakInPuzzle = Preferences.getDidSpeakInPuzzle ()
          LastVolumePressOrPanGestureMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds() }

    let cmd =
        async {
            let! locales = Speech.loadLocales ()
            let localesMsg = LocalesLoaded locales
            return localesMsg
        }
        |> Cmd.ofAsyncMsg

    let init () = initModel, cmd

    // update model

    let update msg modelOld =

        let { SelectedPage = selectedPageOld
              Locales = localesOld
              IsDisplayBoardEnabled = isDisplayBoardEnabledOld
              ConfigOptions = configOptionsOld
              CurrentGame = currentGameOld
              CurrentMoveIndex = currentMoveIndexOld
              CurrentAnnouncementIndex = currentAnnouncementIndexOld
              LastVolumePressOrPanGestureMillis = lastVolumePressOrPanGestureMillisOld } =
            modelOld

        let { AreCoordsEnabled = areCoordsEnabledOld
              AreSymbolsEnabled = areSymbolsEnabledOld
              LocaleIndex = localeIndexOld
              SpeechPitch = speechPitchOld } =
            configOptionsOld

        let { CategoryId = categoryIdOld
              Level = levelOld
              IndexInLevel = indexInLevelOld
              Boards = boardsOld
              Announcements = announcementsOld } =
            currentGameOld

        match msg with
        | NoOp -> modelOld, Cmd.none
        | KeyCodeMessage keyCodeResult ->
            let msgNew =
                match keyCodeResult with
                | VolumeUpCodeResult -> VolumeUpPressed
                | VolumeDownCodeResult -> VolumeDownPressed
                | BackCodeResult -> BackPressed
                | UnknownCodeResult -> NoOp

            modelOld, Cmd.ofMsg msgNew
        | LocalesLoaded v ->
            let cmd =
                async {
                    do! Async.Sleep 5000
                    return StopSponsorDisplay
                }
                |> Cmd.ofAsyncMsg

            { modelOld with Locales = v }, cmd

        | StopSponsorDisplay -> { modelOld with IsSponsorTime = false }, Cmd.none

        | SelectPage v ->
            let categoryIdNew =
                match v with
                | EndgamePuzzlesPage -> 0
                | OpeningPuzzlesPage -> 1
                | _ -> categoryIdOld

            let levelNew, indexInLevelNew =
                match categoryIdNew with
                | 0 -> Preferences.getLevelEndgame (), Preferences.getIndexInLevelEndgame ()
                | 1 -> Preferences.getLevelOpening (), Preferences.getIndexInLevelOpening ()
                | _ -> raise WrongCategoryId

            let modelNew =
                { modelOld with
                    SelectedPage = v
                    CurrentMoveIndex = None
                    IsPuzzleSolved = false
                    CurrentAnnouncementIndex = 0
                    CurrentGame = DB.currentGame areSymbolsEnabledOld categoryIdNew levelNew indexInLevelNew }

            modelNew, Cmd.none

        | GoToMsg goToTarget ->
            let levelNew, indexInLevelNew =
                match goToTarget with
                | NextLevel ->
                    let level =
                        if levelOld > (Constants.numOfLevelsPerCategory - 2) then
                            0
                        else
                            levelOld + 1

                    level, indexInLevelOld
                | PrevLevel ->
                    let level =
                        if levelOld < 1 then
                            Constants.numOfLevelsPerCategory - 1
                        else
                            levelOld - 1

                    level, indexInLevelOld
                | NextPuzzle ->
                    let doesLevelChange = indexInLevelOld > (Constants.numOfPuzzlesPerLevel - 2)

                    let level =
                        match doesLevelChange, levelOld > (Constants.numOfLevelsPerCategory - 2) with
                        | false, _ -> levelOld
                        | true, false -> levelOld + 1
                        | true, true -> 0

                    let indexInLevel = if doesLevelChange then 0 else indexInLevelOld + 1

                    level, indexInLevel
                | PrevPuzzle ->
                    let doesLevelChange = indexInLevelOld < 1

                    let level =
                        match doesLevelChange, levelOld < 1 with
                        | false, _ -> levelOld
                        | true, false -> levelOld - 1
                        | true, true -> Constants.numOfLevelsPerCategory - 1

                    let indexInLevel =
                        if doesLevelChange then
                            Constants.numOfPuzzlesPerLevel - 1
                        else
                            indexInLevelOld - 1

                    level, indexInLevel
                | _ -> levelOld, indexInLevelOld

            let currentMoveIndexNew =
                match goToTarget with
                | NextPos ->
                    match currentMoveIndexOld with
                    | None -> Some 0
                    | Some i when i = boardsOld.Length - 1 -> currentMoveIndexOld
                    | Some i -> Some(i + 1)
                | PrevPos ->
                    match currentMoveIndexOld with
                    | None -> None
                    | Some 0 -> None
                    | Some i -> Some(i - 1)
                | LastPos -> Some(boardsOld.Length - 1)
                | InitPos -> None
                | _ -> None

            let currentGameNew, isPuzzleSolvedNew, currentAnnouncementIndexNew =
                match goToTarget with
                | NextLevel
                | PrevLevel
                | NextPuzzle
                | PrevPuzzle -> DB.currentGame areSymbolsEnabledOld categoryIdOld levelNew indexInLevelNew, false, 0
                | _ -> currentGameOld, true, currentAnnouncementIndexOld

            match categoryIdOld with
            | 0 ->
                Preferences.setLevelEndgame levelNew
                Preferences.setIndexInLevelEndgame indexInLevelNew
            | 1 ->
                Preferences.setLevelOpening levelNew
                Preferences.setIndexInLevelOpening indexInLevelNew
            | _ -> raise WrongCategoryId

            let modelNew =
                { modelOld with
                    CurrentMoveIndex = currentMoveIndexNew
                    CurrentAnnouncementIndex = currentAnnouncementIndexNew
                    CurrentGame = currentGameNew
                    IsPuzzleSolved = isPuzzleSolvedNew }

            modelNew, Cmd.none

        | Speak v ->
            let cmd =
                async {
                    do! Speech.speak speechPitchOld localesOld localeIndexOld v
                    return None
                }
                |> Cmd.ofAsyncMsgOption

            modelOld, cmd

        | ShowSolution ->
            let currentMoveIndexNew =
                match categoryIdOld with
                | 0 -> None
                | 1 -> Some(boardsOld.Length - 2)
                | _ -> raise WrongCategoryId

            let modelNew =
                { modelOld with
                    CurrentMoveIndex = currentMoveIndexNew
                    IsPuzzleSolved = true }

            modelNew, Cmd.none

        | BackPressed ->
            if selectedPageOld = HomePage then
                Environment.Exit(0)
            else
                ()

            let modelNew =
                { modelOld with
                    SelectedPage = HomePage }

            modelNew, Cmd.none

        | VolumeUpPressed
        | PanRightGesture ->
            let currentMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds()

            let isDebounceThresholdOver =
                currentMillis - lastVolumePressOrPanGestureMillisOld > Constants.volumePressOrPanGestureDebounceTimeout

            match isDebounceThresholdOver, selectedPageOld with
            | true, OpeningPuzzlesPage
            | true, EndgamePuzzlesPage ->
                Preferences.setDidSpeakInPuzzle true
                let cmd = announcementsOld[0] |> Speak |> Cmd.ofMsg

                let modelNew =
                    { modelOld with
                        DidSpeakInPuzzle = true
                        CurrentAnnouncementIndex = 1
                        LastVolumePressOrPanGestureMillis = currentMillis }

                modelNew, cmd
            | _ -> modelOld, Cmd.none

        | VolumeDownPressed
        | PanLeftGesture ->
            let currentMillis = DateTimeOffset.Now.ToUnixTimeMilliseconds()

            let isDebounceThresholdOver =
                currentMillis - lastVolumePressOrPanGestureMillisOld > Constants.volumePressOrPanGestureDebounceTimeout

            match isDebounceThresholdOver, selectedPageOld with
            | true, OpeningPuzzlesPage
            | true, EndgamePuzzlesPage ->
                match currentAnnouncementIndexOld with
                | v when v = announcementsOld.Length ->
                    let modelNew =
                        { modelOld with
                            DidSpeakInPuzzle = true
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    Preferences.setDidSpeakInPuzzle true
                    let firstCmd = "next puzzle" |> Speak |> Cmd.ofMsg
                    let secondCmd = Cmd.ofMsg (GoToMsg NextPuzzle)
                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    modelNew, cmd
                | v when v = announcementsOld.Length - 1 ->
                    let modelNew =
                        { modelOld with
                            DidSpeakInPuzzle = true
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let firstCmd = Cmd.ofMsg ShowSolution

                    Preferences.setDidSpeakInPuzzle true
                    let secondCmd = announcementsOld[v] |> Speak |> Cmd.ofMsg

                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    modelNew, cmd
                | v ->
                    let modelNew =
                        { modelOld with
                            DidSpeakInPuzzle = true
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    Preferences.setDidSpeakInPuzzle true
                    let cmd = announcementsOld[v] |> Speak |> Cmd.ofMsg

                    modelNew, cmd
            | _ -> modelOld, Cmd.none

        | SwitchIsDisplayBoardEnabled ->
            let v = not isDisplayBoardEnabledOld
            Preferences.setIsDisplayBoardEnabled v

            let modelNew =
                { modelOld with
                    IsDisplayBoardEnabled = v }

            modelNew, Cmd.none

        | SetConfig configMsg ->
            let configOptionsNew =

                match configMsg with

                | SwitchAreCoordsEnabled ->
                    let v = not areCoordsEnabledOld
                    Preferences.setAreCoordsEnabled v

                    { configOptionsOld with
                        AreCoordsEnabled = v }

                | SwitchAreSymbolsEnabled ->
                    let v = not areSymbolsEnabledOld
                    Preferences.setAreSymbolsEnabled v

                    { configOptionsOld with
                        AreSymbolsEnabled = v }

                | SetBoardSizeRatio v ->
                    Preferences.setBoardSizeRatio v

                    { configOptionsOld with
                        BoardSizeRatio = v }

                | SetFontSizeRatio v ->
                    Preferences.setFontSizeRatio v

                    { configOptionsOld with
                        FontSizeRatio = v }
                | SetSpeechPitch v ->
                    Preferences.setSpeechPitch v

                    { configOptionsOld with
                        SpeechPitch = v }
                | SetLocaleIndex v ->
                    Preferences.setLocaleIndex v

                    { configOptionsOld with
                        LocaleIndex = v }
                | Reset ->
                    Preferences.resetConfig ()
                    initConfigOptions ()

            let modelNew =
                { modelOld with
                    ConfigOptions = configOptionsNew }

            modelNew, Cmd.none

        | UrlClick externalUrl ->
            match externalUrl with
            | GitHub -> Constants.gitHubUrl
            | LinkedIn -> Constants.linkedInUrl
            | AppStore -> Constants.appStoreUrl
            | PrivacyPolicy -> Constants.privacyPolicyUrl
            |> Uri
            |> Launcher.OpenAsync
            |> ignore

            modelOld, Cmd.none

        | ShareApp ->
            Constants.appStoreUrl |> Share.RequestAsync |> ignore

            modelOld, Cmd.none

    // view model

    let view model =

        Application(
            match model.SelectedPage with
            | HomePage -> Pages.Home.view model
            | EndgamePuzzlesPage -> Pages.EndgamePuzzles.view model
            | OpeningPuzzlesPage -> Pages.OpeningPuzzles.view model
            | DescriptionPage -> Pages.Description.view model
            | OptionsPage -> Pages.Options.view model
            | CreditsPage -> Pages.Credits.view model
        )

    let codeReceivedSubscription _model =
        Cmd.ofSub (fun dispatch ->
            let codeReceivedService = CodeReceivedService.Instance
            codeReceivedService.KeyCodeReceived.Add(fun code -> dispatch (KeyCodeMessage code)))

    let program =
        Program.statefulWithCmd init update view
        |> Program.withSubscription codeReceivedSubscription
