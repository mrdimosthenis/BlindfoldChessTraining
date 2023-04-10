module BlindfoldChessTraining.Constants

open Types
open Xamarin.Forms
open Xamarin.Essentials

let os =
    match Device.RuntimePlatform with
    | Device.Android -> Android
    | Device.iOS -> IOS
    | _ -> raise UnknownOS

let volumePressOrPanGestureDebounceTimeout = 500L

let version = VersionTracking.CurrentVersion

let introWaitMillis = 3000

let numOfLevelsPerCategory = 10
let numOfPuzzlesPerLevel = 50

let appStoreUrl =
    match os with
    | Android -> "https://play.google.com/store/apps/details?id=com.github.mrdimosthenis.blindfoldchesstraining"
    | IOS -> "https://apps.apple.com/us/app/apple-store/id1553271236"

let gitHubUrl = "https://github.com/mrdimosthenis/BlindfoldChessTraining"
let linkedInUrl = "https://www.linkedin.com/in/mrdimosthenis/"

let privacyPolicyUrl =
    "https://github.com/mrdimosthenis/BlindfoldChessTraining/blob/master/privacy_policy.md"
