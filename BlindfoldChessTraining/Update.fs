module BlindfoldChessTraining.Update

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    match msg with
    | Msg.LocalesLoaded v -> { model with Model.Locales = v }, Cmd.none
    | Msg.SelectPage v -> { model with Model.SelectedPage = v }, Cmd.none
    | Msg.SelectCoordsConfig v ->
        let newConfigOptions = { model.ConfigOptions with AreCoordsEnabled = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectPieceSymbolConfig v ->
        let newConfigOptions = { model.ConfigOptions with AreSymbolsEnabled = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectFontSizeConfig v ->
        let newConfigOptions = { model.ConfigOptions with FontSize = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectPitchConfig v ->
        let newConfigOptions = { model.ConfigOptions with SpeechPitch = v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.SelectLocaleConfig v ->
        let newConfigOptions = { model.ConfigOptions with SelectedLocale = Some v }
        { model with Model.ConfigOptions = newConfigOptions }, Cmd.none
    | Msg.ResetConfigs ->
        { model with Model.ConfigOptions = Model.initConfigOptions }, Cmd.none
