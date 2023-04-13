namespace BlindfoldChessTraining

open FSharpx.Collections
open Fabulous
open Fabulous.Maui
open Microsoft.Maui
open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.ApplicationModel.DataTransfer
open System
open type Fabulous.Maui.View
open Types

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
          FontSizeRatio = Preferences.getFontSizeRatio ()
          LocaleIndex = Preferences.getLocaleIndex ()
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
          IsDisplayBoardEnabled = Preferences.getIsDisplayBoardEnabled ()
          ConfigOptions = initConfigOptions ()
          CurrentGame = initCurrentGame
          CurrentMoveIndex = None
          IsPuzzleSolved = false
          CurrentAnnouncementIndex = 0
          DidVolumeNoteClicked = Preferences.getDidVolumeNoteClicked ()
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
        | LocalesLoaded v ->
            let cmd =
                async {
                    do! Async.Sleep Constants.introWaitMillis
                    return SelectPage HomePage
                }
                |> Cmd.ofAsyncMsg

            { modelOld with Locales = v }, cmd

        | SelectPage v ->
            let categoryIdNew =
                match v with
                | EndgamePuzzlesPage -> 0
                | OpeningPuzzlesPage -> 1
                | _ -> categoryIdOld

            let modelNew =
                { modelOld with
                    SelectedPage = v
                    CurrentMoveIndex = None
                    IsPuzzleSolved = false
                    CurrentAnnouncementIndex = 0
                    CurrentGame = DB.currentGame areSymbolsEnabledOld categoryIdNew levelOld indexInLevelOld }

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
                | NextMove ->
                    match currentMoveIndexOld with
                    | None -> Some 0
                    | Some i when i = boardsOld.Length - 1 -> currentMoveIndexOld
                    | Some i -> Some(i + 1)
                | PrevMove ->
                    match currentMoveIndexOld with
                    | None -> None
                    | Some 0 -> None
                    | Some i -> Some(i - 1)
                | LastPos -> Some(boardsOld.Length - 1)
                | _ -> None

            let currentGameNew =
                match goToTarget with
                | NextLevel
                | PrevLevel
                | NextPuzzle
                | PrevPuzzle -> DB.currentGame areSymbolsEnabledOld categoryIdOld levelNew indexInLevelNew
                | _ -> currentGameOld

            let modelNew =
                { modelOld with
                    CurrentMoveIndex = currentMoveIndexNew
                    CurrentGame = currentGameNew }

            modelNew, Cmd.none

        | Speak v ->
            let cmd =
                async {
                    do! Speech.speak speechPitchOld localesOld localeIndexOld v
                    return None
                }
                |> Cmd.ofAsyncMsgOption

            modelOld, cmd

        | VolumeNoteClicked ->
            Preferences.setDidVolumeNoteClicked true

            { modelOld with
                DidVolumeNoteClicked = true },
            Cmd.ofMsg VolumeUpPressed

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
                System.Diagnostics.Process.GetCurrentProcess().CloseMainWindow() |> ignore
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
                let cmd = announcementsOld[0] |> Speak |> Cmd.ofMsg

                let modelNew =
                    { modelOld with
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
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let firstCmd = "next puzzle" |> Speak |> Cmd.ofMsg
                    let secondCmd = Cmd.ofMsg (GoToMsg NextPuzzle)
                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    modelNew, cmd
                | v when v = announcementsOld.Length - 1 ->
                    let modelNew =
                        { modelOld with
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

                    let firstCmd = Cmd.ofMsg ShowSolution

                    let secondCmd = announcementsOld[v] |> Speak |> Cmd.ofMsg

                    let cmd = Cmd.batch [ firstCmd; secondCmd ]
                    modelNew, cmd
                | v ->
                    let modelNew =
                        { modelOld with
                            CurrentAnnouncementIndex = v + 1
                            LastVolumePressOrPanGestureMillis = currentMillis }

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

                | SetFontSizeRatio v ->
                    Preferences.setFontSizeRatio v

                    { configOptionsOld with
                        FontSizeRatio = v }
                | SetSpeechPitch v ->
                    Preferences.setSpeechPitch v

                    { configOptionsOld with
                        SpeechPitch = v }
                | SetSelectedLocaleIndex v ->
                    Preferences.setLocaleIndex v

                    { configOptionsOld with
                        LocaleIndex = Some v }
                | Reset ->
                    Preferences.reset ()
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
            Constants.appStoreUrl
            |> Share.RequestAsync
            |> ignore

            modelOld, Cmd.none

    // view model

    let view _model =
        Application(
            ContentPage(
                ScrollView(
                    (VStack(spacing = 25.) {
                        Image("dotnet_bot.png")
                            .semantics(description = "Cute dotnet bot waving hi to you!")
                            .height(200.)
                            .centerHorizontal ()

                        Image("logos/main.png")
                            .semantics(description = "Cute dotnet bot waving hi to you!")
                            .height(200.)
                            .centerHorizontal ()

                        Label("Hello , World!")
                            .semantics(SemanticHeadingLevel.Level1)
                            .font(size = 32.)
                            .centerTextHorizontal ()

                        Label("Welcome to .NET Multi-platform App UI powered by Fabulous")
                            .semantics(
                                SemanticHeadingLevel.Level2,
                                "Welcome to dot net Multi platform App U I powered by Fabulous"
                            )
                            .font(size = 18.)
                            .centerTextHorizontal ()

                        Button("Share", UrlClick GitHub)
                            .semantics(hint = "Shares the app")
                            .centerHorizontal ()
                    })
                        .padding(30., 0., 30., 0.)
                        .centerVertical ()
                )
            )
        )

    let program = Program.statefulWithCmd init update view
