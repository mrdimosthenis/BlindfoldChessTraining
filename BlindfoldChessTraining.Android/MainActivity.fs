﻿// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace BlindfoldChessTraining.Android

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.Runtime
open Android.Views
open Android.Widget
open Android.OS
open Xamarin.Forms.Platform.Android

open BlindfoldChessTraining

[<Activity (Label = "Blindfold Chess Training", Icon = "@drawable/icon", Theme = "@style/MainTheme", MainLauncher = true, ConfigurationChanges = (ConfigChanges.ScreenSize ||| ConfigChanges.Orientation))>]
type MainActivity() =
    inherit FormsAppCompatActivity()

    let mutable _app: App option = None

    override this.OnCreate (bundle: Bundle) =
        FormsAppCompatActivity.TabLayoutResource <- Resources.Layout.Tabbar
        FormsAppCompatActivity.ToolbarResource <- Resources.Layout.Toolbar
        base.OnCreate (bundle)

        Xamarin.Essentials.Platform.Init(this, bundle)

        Xamarin.Forms.Forms.Init (this, bundle)

        let appcore  = new App()
        this.LoadApplication (appcore)

        _app <- Some appcore

    override this.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<GeneratedEnum>] grantResults: Android.Content.PM.Permission[]) =
        Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults)

        base.OnRequestPermissionsResult(requestCode, permissions, grantResults)

    override this.OnKeyDown(keycode: Keycode, _: KeyEvent) =
        match _app with
        | Some appcore ->
                    match keycode with
                    | Keycode.VolumeUp ->
                        appcore.Program.Dispatch Msg.VolumeUpPressed
                    | Keycode.VolumeDown ->
                        appcore.Program.Dispatch Msg.VolumeDownPressed
                    | Keycode.Back ->
                        appcore.Program.Dispatch Msg.BackPressed
                    | _ -> ()
                    true
        | None -> true
