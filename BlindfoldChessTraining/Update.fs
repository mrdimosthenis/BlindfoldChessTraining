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
    let newGameWithBoards = newGameJsonStr |> Notation.Parser.jsonOfGame |> Model.gameToGameWithBoards
    match model.SelectedPage with
    | Model.OpeningPuzzlesPage ->
        Preferences.setString Preferences.openingJsonStrKey newGameJsonStr
        { model with CurrentGameWithBoards = newGameWithBoards
                     CurrentMoveIndex = None
                     OpeningJsonStr = newGameJsonStr }
    | _ ->
        Preferences.setString Preferences.endgameJsonStrKey newGameJsonStr
        { model with CurrentGameWithBoards = newGameWithBoards
                     CurrentMoveIndex = None
                     EndgameJsonStr = newGameJsonStr }

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    match msg with
    | Msg.LocalesLoaded v ->
        let cmd = async {
                    do! Async.Sleep Constants.introWaitMillis
                    return Msg.SelectPage Model.HomePage
                  } |> Cmd.ofAsyncMsg
        { model with Model.Locales = v }, cmd

    | Msg.SelectPage v ->
        let currentGameWithBoards =
                match v with
                | Model.OpeningPuzzlesPage -> Notation.Parser.jsonOfGame model.OpeningJsonStr
                | _ -> Notation.Parser.jsonOfGame model.EndgameJsonStr
                |> Model.gameToGameWithBoards
        let newModel = { model with Model.SelectedPage = v
                                    Model.CurrentGameWithBoards = currentGameWithBoards
                                    CurrentMoveIndex = None }
        newModel, Cmd.none

    | Msg.GoToPrevLevel ->
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = if model.CurrentGameWithBoards.Level < 1 then Constants.numOfLevelsPerCategory - 1
                    else model.CurrentGameWithBoards.Level - 1
        let indexInLevel = model.CurrentGameWithBoards.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToNextLevel ->
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = if model.CurrentGameWithBoards.Level > (Constants.numOfLevelsPerCategory - 2) then 0
                    else model.CurrentGameWithBoards.Level + 1
        let indexInLevel = model.CurrentGameWithBoards.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToPrevPuzzle ->
        let doesLevelChange = model.CurrentGameWithBoards.IndexInLevel < 1
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = match (doesLevelChange, model.CurrentGameWithBoards.Level < 1) with
                    | (false, _) -> model.CurrentGameWithBoards.Level
                    | (true, false) -> model.CurrentGameWithBoards.Level - 1
                    | (true, true) -> Constants.numOfLevelsPerCategory - 1
        let indexInLevel = if doesLevelChange then Constants.numOfPuzzlesPerLevel - 1
                           else model.CurrentGameWithBoards.IndexInLevel - 1
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | Msg.GoToNextPuzzle ->
        let doesLevelChange = model.CurrentGameWithBoards.IndexInLevel > (Constants.numOfPuzzlesPerLevel - 2)
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = match (doesLevelChange, model.CurrentGameWithBoards.Level > (Constants.numOfLevelsPerCategory - 2)) with
                    | (false, _) -> model.CurrentGameWithBoards.Level
                    | (true, false) -> model.CurrentGameWithBoards.Level + 1
                    | (true, true) -> 0
        let indexInLevel = if doesLevelChange then 0
                           else model.CurrentGameWithBoards.IndexInLevel + 1
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none

    | Msg.GoToNextMove ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None ->
                                        Some 0
                                  | Some i when i = model.CurrentGameWithBoards.MovesWithBoards.Length - 1 ->
                                        model.CurrentMoveIndex
                                  | Some i ->
                                        Some (i + 1)
        { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | Msg.GoToPrevMove ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None -> None
                                  | Some 0 -> None
                                  | Some i -> Some (i - 1)
        { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | Msg.GoToInitPos ->
        { model with Model.CurrentMoveIndex = None }, Cmd.none
    | Msg.GoToLastPos ->
        let newCurrentMoveIndex = Some (model.CurrentGameWithBoards.MovesWithBoards.Length - 1)
        { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none

    | Msg.Speak v ->
        Speech.speak model.ConfigOptions.SpeechPitch
                     model.Locales
                     model.ConfigOptions.SelectedLocale
                     v
        model, Cmd.none

    | Msg.SelectCoordsConfig v ->
        Preferences.setBool Preferences.areCoordsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreCoordsEnabled = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectPieceSymbolConfig v ->
        Preferences.setBool Preferences.areSymbolsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreSymbolsEnabled = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectFontSizeConfig v ->
        Preferences.setFloat Preferences.fontSizeKey v
        let newConfigOptions = { model.ConfigOptions with FontSize = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectPitchConfig v ->
        Preferences.setFloat Preferences.speechPitchKey v
        let newConfigOptions = { model.ConfigOptions with SpeechPitch = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectLocaleConfig v ->
        Preferences.setInt Preferences.selectedLocaleKey v
        let newConfigOptions = { model.ConfigOptions with SelectedLocale = Some v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.ResetConfigs ->
        Model.resetConfigOptions()
        { model with Model.ConfigOptions = Model.initConfigOptions() }, Cmd.none
