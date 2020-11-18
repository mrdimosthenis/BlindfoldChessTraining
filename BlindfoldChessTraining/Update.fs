module BlindfoldChessTraining.Update

open Fabulous
open BlindfoldChessMechanics

let cmdInit(): Cmd<Msg.Msg> =
    async {
        let! locales = Speech.loadLocales()
        let localesMsg = Msg.LocalesLoaded locales
        return localesMsg
    } |> Cmd.ofAsyncMsg

let getNewGameFromDBAndModel (model: Model.Model) (categoryId: int, level: int, indexInLevel: int): Model.Model =
    let newGameJsonStr = DB.getGameJsonStr(categoryId, level, indexInLevel)
    let newCurrentGame =
            newGameJsonStr
            |> Notation.Parser.jsonOfGame
            |> Model.mechanicToCurrentGame model.ConfigOptions.AreSymbolsEnabled model.SelectedPage
    match model.SelectedPage with
    | Model.OpeningPuzzlesPage ->
        Preferences.setString Preferences.openingJsonStrKey newGameJsonStr
        { model with CurrentGame = newCurrentGame
                     CurrentMoveIndex = None
                     OpeningJsonStr = newGameJsonStr
                     IsPuzzleSolved = false
                     CurrentAnnouncementIndex = 0 }
    | _ ->
        Preferences.setString Preferences.endgameJsonStrKey newGameJsonStr
        { model with CurrentGame = newCurrentGame
                     CurrentMoveIndex = None
                     EndgameJsonStr = newGameJsonStr
                     IsPuzzleSolved = false
                     CurrentAnnouncementIndex = 0 }

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    match msg with
    | Msg.LocalesLoaded v ->
        let cmd = async {
                    do! Async.Sleep Constants.introWaitMillis
                    return Msg.SelectPage Model.HomePage
                  } |> Cmd.ofAsyncMsg
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
        let newModel = { model with SelectedPage = v
                                    CurrentGame = newCurrentGame
                                    CurrentMoveIndex = None
                                    IsPuzzleSolved = false
                                    CurrentAnnouncementIndex = 0 }
        newModel, Cmd.none

    | Msg.GoToPrevLevel ->
        let categoryId = model.CurrentGame.CategoryId
        let level = if model.CurrentGame.Level < 1 then Constants.numOfLevelsPerCategory - 1
                    else model.CurrentGame.Level - 1
        let indexInLevel = model.CurrentGame.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToNextLevel ->
        let categoryId = model.CurrentGame.CategoryId
        let level = if model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2) then 0
                    else model.CurrentGame.Level + 1
        let indexInLevel = model.CurrentGame.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToPrevPuzzle ->
        let doesLevelChange = model.CurrentGame.IndexInLevel < 1
        let categoryId = model.CurrentGame.CategoryId
        let level = match (doesLevelChange, model.CurrentGame.Level < 1) with
                    | (false, _) -> model.CurrentGame.Level
                    | (true, false) -> model.CurrentGame.Level - 1
                    | (true, true) -> Constants.numOfLevelsPerCategory - 1
        let indexInLevel = if doesLevelChange then Constants.numOfPuzzlesPerLevel - 1
                           else model.CurrentGame.IndexInLevel - 1
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToNextPuzzle ->
        let doesLevelChange = model.CurrentGame.IndexInLevel > (Constants.numOfPuzzlesPerLevel - 2)
        let categoryId = model.CurrentGame.CategoryId
        let level = match (doesLevelChange, model.CurrentGame.Level > (Constants.numOfLevelsPerCategory - 2)) with
                    | (false, _) -> model.CurrentGame.Level
                    | (true, false) -> model.CurrentGame.Level + 1
                    | (true, true) -> 0
        let indexInLevel = if doesLevelChange then 0
                           else model.CurrentGame.IndexInLevel + 1
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none

    | Msg.GoToNextMove ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None ->
                                        Some 0
                                  | Some i when i = model.CurrentGame.Boards.Length - 1 ->
                                        model.CurrentMoveIndex
                                  | Some i ->
                                        Some (i + 1)
        { model with CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | Msg.GoToPrevMove ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None -> None
                                  | Some 0 -> None
                                  | Some i -> Some (i - 1)
        { model with CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | Msg.GoToInitPos ->
        { model with CurrentMoveIndex = None }, Cmd.none
    | Msg.GoToLastPos ->
        let newCurrentMoveIndex = Some (model.CurrentGame.Boards.Length - 1)
        { model with CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none

    | Msg.Speak v ->
        let cmd = async {
                    do! Speech.speak model.ConfigOptions.SpeechPitch
                                     model.Locales
                                     model.ConfigOptions.SelectedLocale
                                     v
                    return None
                  } |> Cmd.ofAsyncMsgOption
        model, cmd

    | Msg.VolumeNoteClicked ->
        Preferences.setBool Preferences.didVolumeNoteClickedKey true
        { model with DidVolumeNoteClicked = true }, Cmd.ofMsg Msg.VolumeUpPressed

    | Msg.ShowSolution ->
        let newCurrentMoveIndex =
                match model.SelectedPage with
                | Model.OpeningPuzzlesPage ->
                    Some (model.CurrentGame.Boards.Length - 2)
                | _ ->
                    None
        let newModel = { model with CurrentMoveIndex = newCurrentMoveIndex 
                                    IsPuzzleSolved = true }
        newModel, Cmd.none

    | Msg.BackPressed ->
        if model.SelectedPage = Model.HomePage
            then System.Diagnostics.Process.GetCurrentProcess().CloseMainWindow() |> ignore
            else ()
        { model with SelectedPage = Model.HomePage }, Cmd.none

    | Msg.VolumeUpPressed ->
        match model.SelectedPage with
        | Model.OpeningPuzzlesPage | Model.EndgamePuzzlesPage ->
            let cmd = model.CurrentGame.Announcements.[0] |> Msg.Speak |> Cmd.ofMsg
            { model with CurrentAnnouncementIndex = 1 }, cmd
        | _ ->
            model, Cmd.none

    | Msg.VolumeDownPressed ->
        match model.SelectedPage with
        | Model.OpeningPuzzlesPage | Model.EndgamePuzzlesPage ->
            match model.CurrentAnnouncementIndex with
            | v when v = model.CurrentGame.Announcements.Length ->
                let firstCmd = "next puzzle" |> Msg.Speak |> Cmd.ofMsg
                let secondCmd = Cmd.ofMsg Msg.GoToNextPuzzle
                let cmd = Cmd.batch [ firstCmd; secondCmd ]
                model, cmd
            | v when v = model.CurrentGame.Announcements.Length - 1 ->
                let newModel = { model with CurrentAnnouncementIndex = v + 1 }
                let firstCmd = Cmd.ofMsg Msg.ShowSolution
                let secondCmd = model.CurrentGame.Announcements.[v] |> Msg.Speak |> Cmd.ofMsg
                let cmd = Cmd.batch [ firstCmd; secondCmd ]
                newModel, cmd
            | v ->
                let newModel = { model with CurrentAnnouncementIndex = v + 1 }
                let cmd = model.CurrentGame.Announcements.[v] |> Msg.Speak |> Cmd.ofMsg
                newModel, cmd
        | _ ->
            model, Cmd.none

    | Msg.SelectDisplayBoardOption v ->
        Preferences.setBool Preferences.isDisplayBoardOptionEnabledKey v
        { model with IsDisplayBoardOptionEnabled = v }, Cmd.none

    | Msg.SelectCoordsConfig v ->
        Preferences.setBool Preferences.areCoordsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreCoordsEnabled = v }
        let cmd = model.SelectedPage |> Msg.SelectPage |> Cmd.ofMsg
        { model with ConfigOptions = newConfigOptions }, cmd
    | Msg.SelectBoardSizeConfig v ->
        Preferences.setFloat Preferences.boardSizeKey v
        let newConfigOptions = { model.ConfigOptions with BoardSize = v }
        let cmd1 = Model.IntroPage |> Msg.SelectPage |> Cmd.ofMsg
        let cmd2 = Model.OptionsPage |> Msg.SelectPage |> Cmd.ofMsg
        let cmd = Cmd.batch [ cmd1; cmd2 ]
        { model with ConfigOptions = newConfigOptions }, cmd
    | Msg.SelectPieceSymbolConfig v ->
        Preferences.setBool Preferences.areSymbolsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreSymbolsEnabled = v }
        { model with ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectFontSizeConfig v ->
        Preferences.setFloat Preferences.fontSizeKey v
        let newConfigOptions = { model.ConfigOptions with FontSize = v }
        { model with ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectPitchConfig v ->
        Preferences.setFloat Preferences.speechPitchKey v
        let newConfigOptions = { model.ConfigOptions with SpeechPitch = v }
        { model with ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectLocaleConfig v ->
        Preferences.setInt Preferences.selectedLocaleKey v
        let newConfigOptions = { model.ConfigOptions with SelectedLocale = Some v }
        { model with ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.ResetConfigs ->
        Model.resetConfigOptions()
        { model with ConfigOptions = Model.initConfigOptions() }, Cmd.none
