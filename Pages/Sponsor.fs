module BlindfoldChessTraining.Pages.Sponsor

open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open type Fabulous.Maui.View

let view model =
    let fontSizeRatio = model.ConfigOptions.FontSizeRatio

    match model.SponsorDetails with
    | Some { SponsorName = sponsorName
             SponsorImage = sponsorImage } ->
        [ $"Sponsor: {sponsorName}" |> UIElems.Components.label fontSizeRatio |> Lbl
          sponsorImage |> UIElems.Icons.webImg |> Img
          "please wait a few seconds"
          |> UIElems.Components.label fontSizeRatio
          |> Lbl
          true |> ActivityIndicator |> Ind ]
        |> UIElems.Page.template model "" UIElems.Icons.emptyImg
    | None -> Home.view model
