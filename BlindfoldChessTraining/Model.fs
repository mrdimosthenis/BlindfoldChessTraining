module BlindfoldChessTraining.Model

open Xamarin.Essentials

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
      Locales: Locale seq
      ConfigOptions : ConfigOptions }

let init = { SelectedPage = HomePage
             Locales = Seq.empty
             ConfigOptions = initConfigOptions }
