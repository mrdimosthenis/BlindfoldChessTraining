module BlindfoldChessTraining.Constants

open Xamarin.Forms
open Xamarin.Essentials

let isIOSDevice: bool = Device.RuntimePlatform = Device.iOS

let volumePressOrPanGestureDebounceTimeout: int64 = 500L

let version: string = VersionTracking.CurrentVersion

let titleSizeRatio: float = 1.5

let backgroundColor: Color = Color.LightSteelBlue
let lineColor: Color = Color.DarkSlateGray
let textColor: Color = Color.Black

let darkSquareColor: Color = Color.SlateGray
let lightSquareColor: Color = Color.LightCyan

let numOfLevelsPerCategory: int = 10
let numOfPuzzlesPerLevel: int = 50
