module BlindfoldChessTraining.Model

open Xamarin.Essentials

type SelectedPage =
    | HomePage
    | ExamplePage
    | OpeningPuzzlesPage
    | EndgamePuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions = { AreCoordsEnabled : bool
                       SelectedLocale : int option
                       SpeechPitch: float }

let initConfigOptions: ConfigOptions =
    { AreCoordsEnabled = true
      SelectedLocale = None
      SpeechPitch = 1.0 }

type Model = 
    { Count : int
      Step : int
      TimerOn: bool
      SelectedPage: SelectedPage
      Locales: Locale seq
      ConfigOptions : ConfigOptions }

let init = { Count = 0
             Step = 1
             TimerOn=false
             SelectedPage = HomePage
             Locales = Seq.empty
             ConfigOptions = initConfigOptions }
