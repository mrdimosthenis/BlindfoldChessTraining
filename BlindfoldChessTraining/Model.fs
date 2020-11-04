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

// default values

let defaultAreCoordsEnabled: bool = true
let defaultAreSymbolsEnabled: bool = false
let defaultFontSize: float = 17.0
let defaultSpeechPitch: float = 1.0

// functions

let resetConfigOptions(): unit =
    Preferences.removeIfExists Preferences.areCoordsEnabledKey
    Preferences.removeIfExists Preferences.areSymbolsEnabledKey
    Preferences.removeIfExists Preferences.fontSizeKey
    Preferences.removeIfExists Preferences.selectedLocaleKey
    Preferences.removeIfExists Preferences.speechPitchKey

let initConfigOptions(): ConfigOptions =
    { AreCoordsEnabled = Preferences.areCoordsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreCoordsEnabled
      AreSymbolsEnabled = Preferences.areSymbolsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreSymbolsEnabled
      FontSize = Preferences.fontSizeKey |> Preferences.tryGetFloat |> Option.defaultValue defaultFontSize
      SelectedLocale = Preferences.selectedLocaleKey |> Preferences.tryGetInt
      SpeechPitch = Preferences.speechPitchKey |> Preferences.tryGetFloat |> Option.defaultValue defaultSpeechPitch }

let init() = { SelectedPage = HomePage
               Locales = LazyList.empty
               ConfigOptions = initConfigOptions() }
