module BlindfoldChessTraining.Msg

type Msg = 
    | Increment 
    | Decrement 
    | Reset
    | SetStep of int
    | TimerToggled of bool
    | TimedTick
    | SelectPage of Model.SelectedPage
