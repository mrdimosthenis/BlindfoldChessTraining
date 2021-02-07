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

//let appleAppStoreHorizontalLayout (dispatch: Msg.Msg -> unit): ViewElement =
//    let storeBtn =
//        fun () -> Msg.AppleStore |> Msg.UrlClick |> dispatch
//        |> Component.button "App on Apple Store" Icons.info true
//
//    let shareBtn =
//        fun () -> Msg.AppOnAppleStore |> Msg.UrlShare |> dispatch
//        |> Component.imageButton Icons.share
//
//    View.StackLayout
//        (orientation = StackOrientation.Horizontal,
//         horizontalOptions = LayoutOptions.Center,
//         children = [ storeBtn; shareBtn ])

let privacyPolicyBtn (dispatch: Msg.Msg -> unit): ViewElement =
    fun () -> Msg.PrivacyPolicy |> Msg.UrlClick |> dispatch
    |> Component.button "Privacy Policy" Icons.document true
    
let analyticsHorizontalLayout (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    let shortDescriptionLbl =
        "Send anonymous usage statistics"
        |> Component.label model false

    let checkBox =
        View.CheckBox(
            isChecked = model.AreAnalyticsEnabled,
            checkedChanged = (fun _ -> dispatch Msg.SwitchAnalytics)
        )

    View.StackLayout
        (orientation = StackOrientation.Horizontal,
         horizontalOptions = LayoutOptions.Center,
         children = [ shortDescriptionLbl; checkBox ])

let versionLbl (model: Model.Model): ViewElement =
    Constants.version
    |> sprintf "Version: %s"
    |> Component.label model false

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =

    [ Component.label model false "Blindfold Chess Training was created by Dimosthenis Michailidis"
      codeBtn dispatch
      devBtn dispatch
      Component.separator ()
      googleAppStoreHorizontalLayout dispatch
      //appleAppStoreHorizontalLayout dispatch
      Component.separator ()
      privacyPolicyBtn dispatch
      analyticsHorizontalLayout model dispatch
      Component.separator ()
      versionLbl model ]
    |> Page.page model dispatch "Credits" Icons.fingerprint
