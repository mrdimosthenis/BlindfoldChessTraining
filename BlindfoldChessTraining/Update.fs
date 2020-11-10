module BlindfoldChessTraining.Update

open Fabulous
open BlindfoldChessMechanics

let getNewGameFromDBAndModel (model: Model.Model) (categoryId: int, level: int, indexInLevel: int): Model.Model option =
    let newGameJsonStr = DB.getGameJsonStr(categoryId, level, indexInLevel)
    let newGameWithBoards = newGameJsonStr |> Notation.Parser.jsonOfGame |> Model.gameToGameWithBoards
    match model.SelectedPage with
    | Model.OpeningPuzzlesPage ->
        Preferences.setString Preferences.openingJsonStrKey newGameJsonStr
        Some { model with CurrentGameWithBoards = newGameWithBoards; CurrentMoveIndex = None; OpeningJsonStr = newGameJsonStr }
    | _ ->
        Preferences.setString Preferences.endgameJsonStrKey newGameJsonStr
        Some { model with CurrentGameWithBoards = newGameWithBoards; CurrentMoveIndex = None; EndgameJsonStr = newGameJsonStr }

let update (msg: Msg.Msg) (modelOpt: Model.Model option):(Model.Model option) * Cmd<Msg.Msg> =
    match (modelOpt, msg) with
    | (_, Msg.PrepareDB) ->
        let cmd = async {
                    do! DB.initializeDBAsync()
                    return Msg.LoadLocales
                  } |> Cmd.ofAsyncMsg
        None, cmd

    | (_, Msg.LoadLocales) ->
        let cmd = async {
                    let! locales = Speech.loadLocalesAsync()
                    return Msg.InitModel locales
                  } |> Cmd.ofAsyncMsg
        None, cmd

    | (_, Msg.InitModel locales) ->
        Model.init(locales), Cmd.none

    | (None, _) ->
        None, Cmd.none

    | (Some model, Msg.SelectPage v) ->
        let currentGameWithBoards =
                match v with
                | Model.OpeningPuzzlesPage -> Notation.Parser.jsonOfGame model.OpeningJsonStr
                | _ -> Notation.Parser.jsonOfGame model.EndgameJsonStr
                |> Model.gameToGameWithBoards
        Some { model with Model.SelectedPage = v; Model.CurrentGameWithBoards = currentGameWithBoards }, Cmd.none

    | (Some model, Msg.GoToPrevLevel) ->
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = if model.CurrentGameWithBoards.Level < 1 then Constants.numOfLevelsPerCategory - 1
                    else model.CurrentGameWithBoards.Level - 1
        let indexInLevel = model.CurrentGameWithBoards.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | (Some model, Msg.GoToNextLevel) ->
        let categoryId = model.CurrentGameWithBoards.CategoryId
        let level = if model.CurrentGameWithBoards.Level > (Constants.numOfLevelsPerCategory - 2) then 0
                    else model.CurrentGameWithBoards.Level + 1
        let indexInLevel = model.CurrentGameWithBoards.IndexInLevel
        let newModel = getNewGameFromDBAndModel model (categoryId, level, indexInLevel)
        newModel, Cmd.none
    | (Some model, Msg.GoToPrevPuzzle) ->
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
    | (Some model, Msg.GoToNextPuzzle) ->
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

    | (Some model, Msg.GoToNextMove) ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None ->
                                        Some 0
                                  | Some i when i = model.CurrentGameWithBoards.MovesWithBoards.Length - 1 ->
                                        model.CurrentMoveIndex
                                  | Some i ->
                                        Some (i + 1)
        Some { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | (Some model, Msg.GoToPrevMove) ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None -> None
                                  | Some 0 -> None
                                  | Some i -> Some (i - 1)
        Some { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none
    | (Some model, Msg.GoToInitPos) ->
        Some { model with Model.CurrentMoveIndex = None }, Cmd.none
    | (Some model, Msg.GoToLastPos) ->
        let newCurrentMoveIndex = Some (model.CurrentGameWithBoards.MovesWithBoards.Length - 1)
        Some { model with Model.CurrentMoveIndex = newCurrentMoveIndex }, Cmd.none

    | (Some model, Msg.Speak v) ->
        Speech.speak model.ConfigOptions.SpeechPitch
                     model.Locales
                     model.ConfigOptions.SelectedLocale
                     v
        Some model, Cmd.none

    | (Some model, Msg.SelectCoordsConfig v) ->
        Preferences.setBool Preferences.areCoordsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreCoordsEnabled = v }
        Some { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | (Some model, Msg.SelectPieceSymbolConfig v) ->
        Preferences.setBool Preferences.areSymbolsEnabledKey v
        let newConfigOptions = { model.ConfigOptions with AreSymbolsEnabled = v }
        Some { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | (Some model, Msg.SelectFontSizeConfig v) ->
        Preferences.setFloat Preferences.fontSizeKey v
        let newConfigOptions = { model.ConfigOptions with FontSize = v }
        Some { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | (Some model, Msg.SelectPitchConfig v) ->
        Preferences.setFloat Preferences.speechPitchKey v
        let newConfigOptions = { model.ConfigOptions with SpeechPitch = v }
        Some { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | (Some model, Msg.SelectLocaleConfig v) ->
        Preferences.setInt Preferences.selectedLocaleKey v
        let newConfigOptions = { model.ConfigOptions with SelectedLocale = Some v }
        Some { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | (Some model, Msg.ResetConfigs) ->
        Model.resetConfigOptions()
        Some { model with Model.ConfigOptions = Model.initConfigOptions() }, Cmd.none
