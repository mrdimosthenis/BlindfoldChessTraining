module BlindfoldChessTraining.Pages.Home

open BlindfoldChessTraining
open BlindfoldChessTraining.Types

let view model =
    let innerElems =
        [ Img UIElems.Icons.mainLogo
          EndgamePuzzlesPage |> SelectPage |> UIElems.Icons.cube "Endgame Puzzles" |> Btn
          OpeningPuzzlesPage
          |> SelectPage
          |> UIElems.Icons.library "Opening Puzzles"
          |> Btn
          DescriptionPage |> SelectPage |> UIElems.Icons.annotation "Description" |> Btn
          OptionsPage |> SelectPage |> UIElems.Icons.options "Options" |> Btn
          CreditsPage |> SelectPage |> UIElems.Icons.fingerprint "Credits" |> Btn ]

    UIElems.Page.template model "Blindfold Chess Training" UIElems.Icons.homeColored innerElems
