module BlindfoldChessTraining.Model

type SelectedPage =
    | HomePage
    | OpeningPuzzlesPage
    | EndgamePuzzlesPage
    | DescriptionPage
    | CreditsPage

type Model = 
    { Count : int
      Step : int
      TimerOn: bool
      SelectedPage: SelectedPage }

let init = { Count = 0
             Step = 1
             TimerOn=false
             SelectedPage = HomePage }
