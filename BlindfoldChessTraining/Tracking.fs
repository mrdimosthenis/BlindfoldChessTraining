module BlindfoldChessTraining.Tracking

open System
open System.Globalization
open FSharpx.Collections

open Xamarin.Forms
open Xamarin.Essentials

open Microsoft.AppCenter
open Microsoft.AppCenter.Analytics
open Microsoft.AppCenter.Crashes

let randomAlphanumeric (): string =
    let chars =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

    seq {
        for _ in 0 .. 7 do
            yield chars.[Random().Next(chars.Length)]
    }
    |> Seq.toArray
    |> (fun x -> String(x))

// safe initialization

let initialize (): unit =
    let areAnalyticsEnabled =
        Preferences.areAnalyticsEnabledKey
        |> Preferences.tryGetBool
        |> Option.defaultValue false

    if areAnalyticsEnabled then
        let keysStr =
            sprintf
                "ios=%s;android=%s"
                Secrets.secrets.["AppCenterIOSSecret"]
                Secrets.secrets.["AppCenterAndroidSecret"]

        try
            AppCenter.Start(keysStr, typeof<Analytics>, typeof<Crashes>)
        with _ -> ()

        try
            Analytics.SetEnabledAsync(true)
            |> Async.AwaitTask
            |> Async.StartImmediate
        with _ -> ()
    else
        ()

// turning off

let stop (): unit =
    try
        Analytics.SetEnabledAsync(false)
        |> Async.AwaitTask
        |> Async.StartImmediate
    with _ -> ()

// dimensions

let properties (): Map<string, string> =
    [ // VersionTracking
      ("appBuild", VersionTracking.CurrentBuild)
      ("appVersion", VersionTracking.CurrentVersion)

      // DeviceInfo
      ("deviceManufacturer", DeviceInfo.Manufacturer)
      ("deviceModel", DeviceInfo.Model)
      ("devicePlatform", DeviceInfo.Platform.ToString())
      ("deviceIdiom", DeviceInfo.Idiom.ToString())
      ("deviceType", DeviceInfo.DeviceType.ToString())
      ("deviceOSVersion", DeviceInfo.VersionString)

      // Device.info
      ("internalScreenWidth", string Device.info.PixelScreenSize.Width)
      ("internalScreenHeight", string Device.info.PixelScreenSize.Height)
      ("internalOrientation",
       match Device.info.CurrentOrientation with
       | Internals.DeviceOrientation.Landscape -> "Landscape"
       | Internals.DeviceOrientation.Portrait -> "Portrait"
       | Internals.DeviceOrientation.LandscapeLeft -> "LandscapeLeft"
       | Internals.DeviceOrientation.LandscapeRight -> "LandscapeRight"
       | Internals.DeviceOrientation.PortraitDown -> "PortraitDown"
       | Internals.DeviceOrientation.PortraitUp -> "PortraitUp"
       | _ -> "Other")

      ("region", RegionInfo.CurrentRegion.TwoLetterISORegionName)
      ("language", CultureInfo.CurrentCulture.TwoLetterISOLanguageName) ]
    |> Map.ofList

// tracking

let track (modelSubMap: Map<string, string>) (eventJsonString: string): unit =
    let props = properties () |> Map.union modelSubMap

    try
        Analytics.TrackEvent(eventJsonString, props)
    with _ -> ()
