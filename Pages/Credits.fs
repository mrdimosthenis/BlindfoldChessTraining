module BlindfoldChessTraining.Pages.Credits

open BlindfoldChessTraining
open BlindfoldChessTraining.Types

let view model =

    let innerElems =
        [ ShareApp |> UIElems.Icons.share "Share the app" |> Btn

          AppStore |> UrlClick |> UIElems.Icons.star "Rate the app" |> Btn

          GitHub |> UrlClick |> UIElems.Icons.code "Code on GitHub" |> Btn

          LinkedIn |> UrlClick |> UIElems.Icons.idCard "Developer on LinkedIn" |> Btn

          PrivacyPolicy |> UrlClick |> UIElems.Icons.document "Privacy Policy" |> Btn

          $"Version: {Constants.version}"
          |> UIElems.Components.label model.ConfigOptions.FontSizeRatio
          |> Lbl ]

    UIElems.Page.template model "Credits" UIElems.Icons.fingerprintColored innerElems
