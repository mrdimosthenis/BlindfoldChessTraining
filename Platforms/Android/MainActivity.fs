namespace BlindfoldChessTraining

open Android.App
open Android.Content.PM
open Android.Views
open BlindfoldChessTraining.Types
open Microsoft.Maui

type KeyCodeReceivedService() =
    let keyCodeReceived = Event<KeyCodeResult>()

    static let _instance = KeyCodeReceivedService()

    static member Current = _instance

    member this.OnKeyCodeReceivedReceived(code: KeyCodeResult) = keyCodeReceived.Trigger(code)

    interface IKeyCodeReceivedService with
        [<CLIEvent>]
        member this.KeyCodeReceived = keyCodeReceived.Publish

[<Activity(Theme = "@style/Maui.SplashTheme",
           MainLauncher = true,
           ConfigurationChanges =
               (ConfigChanges.ScreenSize
                ||| ConfigChanges.Orientation
                ||| ConfigChanges.UiMode
                ||| ConfigChanges.ScreenLayout
                ||| ConfigChanges.SmallestScreenSize
                ||| ConfigChanges.Density),
           ScreenOrientation = ScreenOrientation.Portrait)>]
type MainActivity() =
    inherit MauiAppCompatActivity()

    override this.OnKeyDown(keyCode: Keycode, eventArgs) =
        let keyCodeRes, boolRes =
            match keyCode with
            | Keycode.VolumeUp -> KeyCodeResult.VolumeUpCodeResult, true
            | Keycode.VolumeDown -> KeyCodeResult.VolumeDownCodeResult, true
            | Keycode.Back -> KeyCodeResult.BackCodeResult, true
            | _ -> KeyCodeResult.UnknownCodeResult, base.OnKeyDown(keyCode, eventArgs)

        KeyCodeReceivedService.Current.OnKeyCodeReceivedReceived(keyCodeRes)

        boolRes
