module BlindfoldChessTraining.Model

open Xamarin.Essentials
open FSharpx.Collections

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

let initConfigOptions: ConfigOptions =
    { AreCoordsEnabled = true
      AreSymbolsEnabled = false
      FontSize = 17.0
      SelectedLocale = None
      SpeechPitch = 1.0 }

type Model = 
    { SelectedPage: SelectedPage
      Locales: Locale LazyList
      ConfigOptions : ConfigOptions }

let init = { SelectedPage = HomePage
             Locales = LazyList.empty
             ConfigOptions = initConfigOptions }
