module BlindfoldChessTraining.Model

open Xamarin.Essentials
open FSharpx.Collections

// types

type SelectedPage =
    | HomePage
    | OpeningPuzzlesPage
    | EndgamePuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions = { AreCoordsEnabled: bool
                       AreSymbolsEnabled: bool
                       FontSize: float
                       SelectedLocale: int option
                       SpeechPitch: float }

type Model = 
    { SelectedPage: SelectedPage
      Locales: Locale LazyList
      ConfigOptions : ConfigOptions }

// init values and operations and functions

let defaultAreCoordsEnabled: bool = true
let defaultAreSymbolsEnabled: bool = false
let defaultFontSize: float = 17.0
let defaultSpeechPitch: float = 1.0

let initializeConfigOptions(): unit =
    Preferences.setBool Preferences.areCoordsEnabledKey true
    Preferences.setBool Preferences.areSymbolsEnabledKey false
    Preferences.setFloat Preferences.fontSizeKey 17.0
    Preferences.setFloat Preferences.speechPitchKey 1.0
    Preferences.setString Preferences.versionKey Constants.version

match Preferences.tryGetString(Preferences.versionKey) with
| Some v when v = Constants.version ->
    ()
| _ ->
    Preferences.clear()
    initializeConfigOptions()

let initConfigOptions(): ConfigOptions =
    { AreCoordsEnabled = Preferences.areCoordsEnabledKey |> Preferences.tryGetBool |> Option.get
      AreSymbolsEnabled = Preferences.areSymbolsEnabledKey |> Preferences.tryGetBool |> Option.get
      FontSize = Preferences.fontSizeKey |> Preferences.tryGetFloat |> Option.get
      SelectedLocale = Preferences.tryGetInt Preferences.selectedLocaleKey
      SpeechPitch = Preferences.speechPitchKey |> Preferences.tryGetFloat |> Option.get }

let init() = { SelectedPage = HomePage
               Locales = LazyList.empty
               ConfigOptions = initConfigOptions() }
