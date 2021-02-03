module BlindfoldChessTraining.Page.Credits

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems
open BlindfoldChessTraining.Template

let codeBtn (dispatch: Msg.Msg -> unit): ViewElement =
    fun () -> Msg.GitHub |> Msg.UrlClick |> dispatch
    |> Component.button "Code on GitHub" Icons.code true

let devBtn (dispatch: Msg.Msg -> unit): ViewElement =
    fun () -> Msg.LinkedIn |> Msg.UrlClick |> dispatch
    |> Component.button "Developer on LinkedIn" Icons.idCard true

let googleAppStoreHorizontalLayout (dispatch: Msg.Msg -> unit): ViewElement =
    let storeBtn =
        fun () -> Msg.GooglePlay |> Msg.UrlClick |> dispatch
        |> Component.button "App on Google Play" Icons.play true

    let shareBtn =
        fun () -> Msg.AppOnGooglePlay |> Msg.UrlShare |> dispatch
        |> Component.imageButton Icons.share

    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         children = [ storeBtn; shareBtn ])

let appleAppStoreHorizontalLayout (dispatch: Msg.Msg -> unit): ViewElement =
    let storeBtn =
        fun () -> Msg.AppleStore |> Msg.UrlClick |> dispatch
        |> Component.button "App on Apple Store" Icons.info true

    let shareBtn =
        fun () -> Msg.AppOnAppleStore |> Msg.UrlShare |> dispatch
        |> Component.imageButton Icons.share

    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         children = [ storeBtn; shareBtn ])

let versionLbl (model: Model.Model): ViewElement =
    Constants.version
    |> sprintf "Version: %s"
    |> Component.label model false

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =

    [ Component.label model false "Blindfold Chess Training was created by Dimosthenis Michailidis"
      codeBtn dispatch
      devBtn dispatch
      googleAppStoreHorizontalLayout dispatch
      appleAppStoreHorizontalLayout dispatch
      versionLbl model ]
    |> Page.page model dispatch "Credits" Icons.fingerprint
