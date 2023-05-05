module BlindfoldChessTraining.Types

open BlindfoldChessMechanics
open Fabulous
open Fabulous.Maui
open FSharpx.Collections
open Microsoft.Maui.Media
open type Fabulous.Maui.View

exception UnknownOS
exception WrongCategoryId
exception InvalidBoardRow of int

type OS =
    | Android
    | IOS

// change it on version updates
type PuzzleObject_V_4_0_0() =
    member val CategoryId = 0 with get, set
    member val Level = 0 with get, set
    member val IndexInLevel = 0 with get, set
    member val Game = "" with get, set

type Sponsor =
    { SponsorName: string
      SponsorImage: string }

type SelectedPage =
    | SponsorPage
    | HomePage
    | EndgamePuzzlesPage
    | OpeningPuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions =
    { AreCoordsEnabled: bool
      AreSymbolsEnabled: bool
      BoardSizeRatio: float
      FontSizeRatio: float
      LocaleIndex: int
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
    { SponsorDetails: Sponsor option
      SelectedPage: SelectedPage
      Locales: Locale LazyList
      IsDisplayBoardEnabled: bool
      ConfigOptions: ConfigOptions
      CurrentGame: CurrentGame
      CurrentMoveIndex: int option
      IsPuzzleSolved: bool
      CurrentAnnouncementIndex: int
      DidSpeakInPuzzle: bool
      LastVolumePressOrPanGestureMillis: int64 }

type ConfigMsg =
    | SwitchAreCoordsEnabled
    | SwitchAreSymbolsEnabled
    | SetBoardSizeRatio of float
    | SetFontSizeRatio of float
    | SetSpeechPitch of float32
    | SetLocaleIndex of int
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
    | NextPos
    | PrevPos
    | InitPos
    | LastPos
    
type KeyCodeResult =
    | VolumeUpCodeResult
    | VolumeDownCodeResult
    | BackCodeResult
    | UnknownCodeResult
    
type IKeyCodeReceivedService =
    [<CLIEvent>]
    abstract KeyCodeReceived: IEvent<KeyCodeResult>

type Msg =
    | NoOp
    | LocalesLoaded of Locale LazyList
    | SelectPage of SelectedPage
    | GoToMsg of GoToTarget
    | Speak of string
    | ShowSolution
    | BackPressed
    | VolumeUpPressed
    | PanRightGesture
    | VolumeDownPressed
    | PanLeftGesture
    | SwitchIsDisplayBoardEnabled
    | SetConfig of ConfigMsg
    | UrlClick of ExternalUrl
    | ShareApp
    | KeyCodeMessage of KeyCodeResult

type WidgetFabImage = WidgetBuilder<Msg, IFabImage>

type WidgetFabLayout =
    | HorizSt of WidgetBuilder<Msg, IFabHorizontalStackLayout>
    | VertSt of WidgetBuilder<Msg, IFabVerticalStackLayout>
    | Flx of WidgetBuilder<Msg, IFabFlexLayout>
    | Grd of WidgetBuilder<Msg, IFabGrid>
    | Sld of WidgetBuilder<Msg, IFabSlider>
    | Lbl of WidgetBuilder<Msg, IFabLabel>
    | Btn of WidgetBuilder<Msg, IFabButton>
    | Ind of WidgetBuilder<Msg, IFabActivityIndicator>
    | Img of WidgetFabImage
