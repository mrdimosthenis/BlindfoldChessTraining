module BlindfoldChessTraining.Msg

open Xamarin.Essentials
open FSharpx.Collections

type Msg = 
    | LocalesLoaded of Locale LazyList
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
