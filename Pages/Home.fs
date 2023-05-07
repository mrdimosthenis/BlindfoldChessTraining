module BlindfoldChessTraining.Pages.Home

open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let view model =
    let fontSizeRatio = model.ConfigOptions.FontSizeRatio

    match model.SponsorDetails, model.IsSponsorTime with
    | Some { SponsorName = sponsorName
             SponsorImage = sponsorImage },
      true ->
        [ $"Sponsor: {sponsorName}" |> UIElems.Components.label fontSizeRatio |> Lbl
          sponsorImage |> UIElems.Icons.webImg |> Img
          "please wait a few seconds" |> UIElems.Components.label fontSizeRatio |> Lbl
          true |> ActivityIndicator |> Ind ]
        |> UIElems.Page.template model "" UIElems.Icons.emptyImg
    | _ ->
        [ Img UIElems.Icons.mainLogo
          EndgamePuzzlesPage |> SelectPage |> UIElems.Icons.cube "Endgame Puzzles" |> Btn
          OpeningPuzzlesPage
          |> SelectPage
          |> UIElems.Icons.library "Opening Puzzles"
          |> Btn
          DescriptionPage |> SelectPage |> UIElems.Icons.annotation "Description" |> Btn
          OptionsPage |> SelectPage |> UIElems.Icons.options "Options" |> Btn
          CreditsPage |> SelectPage |> UIElems.Icons.fingerprint "Credits" |> Btn ]
        |> UIElems.Page.template model "Blindfold Chess Training" UIElems.Icons.homeColored
