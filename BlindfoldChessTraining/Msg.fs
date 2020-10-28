module BlindfoldChessTraining.Msg

open Xamarin.Essentials

type Msg = 
    | LocalesLoaded of Locale seq
    | SelectPage of Model.SelectedPage
    | Speak of string
    | SelectCoordsConfig of bool
    | SelectPieceSymbolConfig of bool
    | SelectFontSizeConfig of float
    | SelectPitchConfig of float
    | SelectLocaleConfig of int
    | ResetConfigs
