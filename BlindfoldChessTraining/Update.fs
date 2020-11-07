module BlindfoldChessTraining.Update

open Fabulous
open BlindfoldChessMechanics

let cmdInit(): Cmd<Msg.Msg> =
    async {
        let! locales = Speech.loadLocales()
        let localesMsg = Msg.LocalesLoaded locales
        return localesMsg
    } |> Cmd.ofAsyncMsg

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    match msg with
    | Msg.LocalesLoaded v -> { model with Model.Locales = v }, Cmd.none

    | Msg.SelectPage v ->
        let currentGame = match v with
                          | Model.OpeningPuzzlesPage -> Notation.Parser.jsonOfGame model.OpeningJsonStr
                          | _ -> Notation.Parser.jsonOfGame model.EndgameJsonStr
        { model with Model.SelectedPage = v; Model.CurrentGame = currentGame }, Cmd.none

    | Msg.GoToNextMove ->
        let newCurrentMoveIndex = match model.CurrentMoveIndex with
                                  | None -> Some 0
                                  | Some i when i = model.CurrentGame.Moves.Length - 1 -> model.CurrentMoveIndex
                                  | Some i -> Some (i + 1)
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
        let newCurrentMoveIndex = Some (model.CurrentGame.Moves.Length - 1)
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
