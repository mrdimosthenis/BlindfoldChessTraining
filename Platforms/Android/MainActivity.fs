namespace BlindfoldChessTraining

open Android.App
open Android.Content.PM
open Android.Views
open BlindfoldChessTraining
open Fabulous
open Microsoft.Maui

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

    override this.OnKeyDown(keycode, _) =
        match keycode with
        | Keycode.VolumeUp -> Cmd.ofMsg Types.VolumeUpPressed
        | Keycode.VolumeDown -> Cmd.ofMsg Types.VolumeDownPressed
        | Keycode.Back -> Cmd.ofMsg Types.BackPressed
        | _ -> Cmd.ofMsg Types.NoOp
        |> ignore
        true
