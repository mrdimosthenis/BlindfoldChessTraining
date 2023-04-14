module BlindfoldChessTraining.Constants

open Microsoft.Maui.ApplicationModel
open Microsoft.Maui.Devices
open Microsoft.Maui.Graphics
open Types

let os =
    match DeviceInfo.Current.Platform with
    | v when v = DevicePlatform.Android -> Android
    | v when v = DevicePlatform.iOS -> IOS
    | _ -> raise UnknownOS

let visualWidth =
    DeviceDisplay.MainDisplayInfo.Width / DeviceDisplay.MainDisplayInfo.Density

let volumePressOrPanGestureDebounceTimeout = 500L

let version = VersionTracking.CurrentVersion

let introWaitMillis = 3000

let numOfLevelsPerCategory = 10
let numOfPuzzlesPerLevel = 50

let lightSquareColor = Color.FromRgb(238, 237, 211)
let darkSquareColor = Color.FromRgb(121, 149, 88)
let lightPieceColor = Color.FromRgb(248, 248, 248)
let darkPieceColor = Color.FromRgb(86, 83, 82)

let darkestColor = Color.FromRgb(31, 30, 27)

let appStoreUrl =
    match os with
    | Android -> "https://play.google.com/store/apps/details?id=com.github.mrdimosthenis.blindfoldchesstraining"
    | IOS -> "https://apps.apple.com/us/app/apple-store/id1553271236"

let gitHubUrl = "https://github.com/mrdimosthenis/BlindfoldChessTraining"
let linkedInUrl = "https://www.linkedin.com/in/mrdimosthenis/"

let privacyPolicyUrl =
    "https://github.com/mrdimosthenis/BlindfoldChessTraining/blob/master/privacy_policy.md"
