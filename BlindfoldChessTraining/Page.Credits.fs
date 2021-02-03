module BlindfoldChessTraining.Page.Credits

open Fabulous

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

let codeBtn (dispatch: Msg.Msg -> unit): ViewElement =
    fun () -> Msg.GitHub |> Msg.UrlClick |> dispatch
    |> Component.button "Code on GitHub" Icons.code true

let devBtn (dispatch: Msg.Msg -> unit): ViewElement =
    fun () -> Msg.LinkedIn |> Msg.UrlClick |> dispatch
    |> Component.button "Developer on LinkedIn" Icons.idCard true

let versionLbl (model: Model.Model): ViewElement =
    Constants.version
    |> sprintf "Version: %s"
    |> Component.label model false

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =

    [ Component.label model false "Blindfold Chess Training was created by Dimosthenis Michailidis"
      codeBtn dispatch
      devBtn dispatch
      versionLbl model ]
    |> Page.page model dispatch "Credits" Icons.fingerprint
