module BlindfoldChessTraining.Page.Home

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let innerElems =
            [ View.Image(source = Images.main)
              Component.button "Endgame Puzzles" Icons.cube true (fun () -> Model.EndgamePuzzlesPage |> Msg.SelectPage |> dispatch)
              Component.button "Opening Puzzles" Icons.library true (fun () -> Model.OpeningPuzzlesPage |> Msg.SelectPage |> dispatch)
              Component.button "Description" Icons.questionmark true (fun () -> Model.DescriptionPage |> Msg.SelectPage |> dispatch)
              Component.button "Options" Icons.options true (fun () -> Model.OptionsPage |> Msg.SelectPage |> dispatch)
              Component.button "Credits" Icons.fingerprint true (fun () -> Model.CreditsPage |> Msg.SelectPage |> dispatch) ]
    Page.page model dispatch "Blindfold Chess Training" Icons.home innerElems
