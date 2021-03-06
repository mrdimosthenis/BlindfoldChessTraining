﻿module BlindfoldChessTraining.Msg

open Xamarin.Essentials
open FSharpx.Collections

type ExternalUrl =
    | GitHub
    | LinkedIn
    | GooglePlay
    | AppleStore
    | PrivacyPolicy

type SharableUrl =
    | AppOnGooglePlay
    | AppOnAppleStore

type Msg = 
    | LocalesLoaded of Locale LazyList
    | SelectPage of Model.SelectedPage
    | GoToNextLevel
    | GoToPrevLevel
    | GoToNextPuzzle
    | GoToPrevPuzzle
    | GoToNextMove
    | GoToPrevMove
    | GoToInitPos
    | GoToLastPos
    | Speak of string
    | VolumeNoteClicked
    | ShowSolution
    | SelectDisplayBoardOption of bool
    | SelectCoordsConfig of bool
    | SelectBoardSizeConfig of float
    | SelectPieceSymbolConfig of bool
    | SelectFontSizeConfig of float
    | SelectPitchConfig of float
    | SelectLocaleConfig of int
    | ResetConfigs
    | VolumeUpPressed
    | VolumeDownPressed
    | PanLeftGesture
    | PanRightGesture
    | BackPressed
    | UrlClick of ExternalUrl
    | UrlShare of SharableUrl
    | SwitchAnalytics
