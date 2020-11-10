module BlindfoldChessTraining.Msg

open FSharpx.Collections
open Xamarin.Essentials

type Msg = 
    | PrepareDB
    | LoadLocales
    | InitModel of Locale LazyList
    | SelectPage of Model.SelectedPage
    | GoToNextLevel
    | GoToPrevLevel
    | GoToNextPuzzle
    | GoToPrevPuzzle
    | GoToNextMove
    | GoToPrevMove
    | GoToInitPos
    | GoToLastPos
    | Speak of string
    | SelectCoordsConfig of bool
    | SelectPieceSymbolConfig of bool
    | SelectFontSizeConfig of float
    | SelectPitchConfig of float
    | SelectLocaleConfig of int
    | ResetConfigs
