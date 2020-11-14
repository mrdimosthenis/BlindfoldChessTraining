// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace BlindfoldChessTraining.iOS

open System
open UIKit
open Foundation
open Xamarin.Forms
open Xamarin.Forms.Platform.iOS

open BlindfoldChessTraining

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit FormsApplicationDelegate ()

    let mutable _app: App option = None

    override this.FinishedLaunching (app, options) =

        Forms.Init()
        let appcore = new App()
        this.LoadApplication (appcore)
        
        _app <- Some appcore

        base.FinishedLaunching(app, options)

    //override this.KeyDown(theEvent: NSEvent) =
    //    match _app with
    //    | Some appcore ->
    //                match keycode with
    //                | Keycode.VolumeUp ->
    //                    appcore.Program.Dispatch Msg.VolumeUpPressed
    //                | Keycode.VolumeDown ->
    //                    appcore.Program.Dispatch Msg.VolumeDownPressed
    //                | Keycode.Back ->
    //                    appcore.Program.Dispatch Msg.BackPressed
    //                | _ -> ()
    //                true
    //    | None -> true

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main(args, null, "AppDelegate")
        0

