module BlindfoldChessTraining.Page.Credits

open System
open Fabulous
open Xamarin.Essentials

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

let openGitHub() = new Uri("https://github.com/mrdimosthenis/BlindfoldChessTraining")
                   |> Launcher.OpenAsync
                   |> Async.AwaitTask
                   |> Async.StartImmediate

let openLinkedIn() = new Uri("https://www.linkedin.com/in/mrdimosthenis/")
                     |> Launcher.OpenAsync
                     |> Async.AwaitTask
                     |> Async.StartImmediate

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    
    [ Component.label model false "Blindfold Chess Training was created by Dimosthenis Michailidis"
      Component.button "Source Code" Icons.code true openGitHub
      Component.button "Developer" Icons.idCard true openLinkedIn
      Constants.version |> sprintf "Version: %s" |> Component.label model false ]
    |> Page.page model dispatch "Credits" Icons.fingerprint
