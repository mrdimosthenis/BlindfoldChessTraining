module BlindfoldChessTraining.Msg

open Xamarin.Essentials

type Msg = 
    | Increment 
    | Decrement 
    | Reset
    | SetStep of int
    | TimerToggled of bool
    | TimedTick
    | SelectPage of Model.SelectedPage
    | LocalesLoaded of Locale seq
    | SelectCoordsConfig of bool
    | SelectPitchConfig of float
