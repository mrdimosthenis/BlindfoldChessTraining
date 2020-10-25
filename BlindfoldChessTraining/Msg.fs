module BlindfoldChessTraining.Msg

open Xamarin.Essentials

type Msg = 
    | Increment 
    | Decrement 
    | Reset
    | SetStep of int
    | TimerToggled of bool
    | TimedTick
    | LocalesLoaded of Locale seq
    | SelectPage of Model.SelectedPage
    | SelectCoordsConfig of bool
    | SelectPieceSymbolConfig of bool
    | SelectFontSizeConfig of float
    | SelectPitchConfig of float
    | SelectLocaleConfig of int
    | ResetConfigs
