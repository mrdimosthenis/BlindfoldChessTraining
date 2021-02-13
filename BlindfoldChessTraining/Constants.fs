module BlindfoldChessTraining.Constants

open Xamarin.Forms
open Xamarin.Essentials

let isIOSDevice: bool = Device.RuntimePlatform = Device.iOS

let volumePressOrPanGestureDebounceTimeout: int64 = 500L

let version: string = VersionTracking.CurrentVersion

let introWaitMillis: int = 3000

let titleSizeRatio: float = 1.2

let backgroundColor: Color = Color.LightSteelBlue
let lineColor: Color = Color.DarkSlateGray
let textColor: Color = Color.Black

let darkSquareColor: Color = Color.SlateGray
let lightSquareColor: Color = Color.LightCyan

let numOfLevelsPerCategory: int = 10
let numOfPuzzlesPerLevel: int = 50

let gitHubUrl: string = "https://github.com/mrdimosthenis/BlindfoldChessTraining"
let linkedInUrl: string = "https://www.linkedin.com/in/mrdimosthenis/"
let googlePlayUrl: string = "https://play.google.com/store/apps/details?id=com.github.mrdimosthenis.blindfoldchesstraining"
let appleStoreUrl: string = "https://apps.apple.com/us/app/apple-store/id1553271236"
let privacyPolicyUrl: string = "https://github.com/mrdimosthenis/BlindfoldChessTraining/blob/master/privacy_policy.md"
