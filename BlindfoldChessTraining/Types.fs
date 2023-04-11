module BlindfoldChessTraining.Types

open BlindfoldChessMechanics
open FSharpx.Collections
open Xamarin.Essentials

exception UnknownOS
exception WrongCategoryId

type OS =
    | Android
    | IOS

// change it on version updates
type PuzzleObject_V_4_0_0() =
    member val CategoryId = 0 with get, set
    member val Level = 0 with get, set
    member val IndexInLevel = 0 with get, set
    member val Game = "" with get, set

type SelectedPage =
    | IntroPage
    | HomePage
    | EndgamePuzzlesPage
    | OpeningPuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions =
    { AreCoordsEnabled: bool
      AreSymbolsEnabled: bool
      BoardSize: float
      FontSizeRatio: float
      SelectedLocaleIndex: int option
      SpeechPitch: float32 }

type CurrentGame =
    { CategoryId: int
      Level: int
      IndexInLevel: int
      IsWhiteToMove: bool
      InitBoard: Logic.Board.Board
      MovesWithNumberIndicators: (string * bool) LazyList
      Boards: Logic.Board.Board array
      WhitePieces: string LazyList
      BlackPieces: string LazyList
      Announcements: string array }

type Model =
    { SelectedPage: SelectedPage
      Locales: Locale LazyList
      IsDisplayBoardOptionEnabled: bool
      ConfigOptions: ConfigOptions
      CurrentGame: CurrentGame
      CurrentMoveIndex: int option
      IsPuzzleSolved: bool
      CurrentAnnouncementIndex: int
      DidVolumeNoteClicked: bool
      LastVolumePressOrPanGestureMillis: int64 }

type ConfigMsg =
    | SwitchAreCoordsEnabled
    | SwitchAreSymbolsEnabled
    | SetBoardSize of float
    | SetFontSizeRatio of float
    | SetSpeechPitch of float32
    | SetSelectedLocaleIndex of int
    | Reset

type ExternalUrl =
    | GitHub
    | LinkedIn
    | AppStore
    | PrivacyPolicy

type GoToTarget =
    | NextLevel
    | PrevLevel
    | NextPuzzle
    | PrevPuzzle
    | NextMove
    | PrevMove
    | InitPos
    | LastPos

type Msg =
    | LocalesLoaded of Locale LazyList
    | SelectPage of SelectedPage
    | GoToMsg of GoToTarget
    | Speak of string
    | VolumeNoteClicked
    | ShowSolution
    | SelectDisplayBoardOption of bool
    | SetConfig of ConfigMsg
    | VolumeUpPressed
    | VolumeDownPressed
    | PanLeftGesture
    | PanRightGesture
    | BackPressed
    | UrlClick of ExternalUrl
    | Share
