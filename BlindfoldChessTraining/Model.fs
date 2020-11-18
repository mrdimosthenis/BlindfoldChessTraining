module BlindfoldChessTraining.Model

open Xamarin.Essentials
open FSharpx.Collections
open BlindfoldChessMechanics

// types

type SelectedPage =
    | IntroPage
    | HomePage
    | EndgamePuzzlesPage
    | OpeningPuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions = { AreCoordsEnabled: bool
                       BoardSize: float
                       AreSymbolsEnabled: bool
                       FontSize: float
                       SelectedLocale: int option
                       SpeechPitch: float }

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
          Announcements : string array }

type Model = 
    { SelectedPage: SelectedPage
      Locales: Locale LazyList
      IsDisplayBoardOptionEnabled: bool
      ConfigOptions : ConfigOptions
      EndgameJsonStr: string
      OpeningJsonStr: string
      CurrentGame: CurrentGame
      CurrentMoveIndex: int option
      IsPuzzleSolved: bool
      CurrentAnnouncementIndex: int
      DidVolumeNoteClicked: bool }

// default values

let defaultIsDisplayBoardOptionEnabled: bool = true

let defaultAreCoordsEnabled: bool = true
let defaultBoardSize: float = 1.0
let defaultAreSymbolsEnabled: bool = false
let defaultFontSize: float = 17.0
let defaultSpeechPitch: float = 1.0

let defaultDidVolumeNoteClicked: bool = false

let defaultEndgameJsonStr: string = DB.getGameJsonStr(0, 0, 0)
let defaultOpeningJsonStr: string = DB.getGameJsonStr(1, 0, 0)

// functions

let mechanicToCurrentGame (areSymbolsEnabled: bool) (selectedPage: SelectedPage) (game: Logic.Game.Game): CurrentGame =
    let categoryId = game.MetaTags.Item("category_id") |> int
    let level = game.MetaTags.Item("level") |> int
    let indexInLevel = game.MetaTags.Item("index_in_level") |> int
    let isWhiteToMove = game.InitialPosition.IsWhiteToMove
    let initBoard = game.InitialPosition.Board
    let movesWithBoards =
            game.Moves
            |> LazyList.ofArray
            |> LazyList.fold
                      (fun acc x ->
                          let prevPos =
                              if LazyList.isEmpty acc then game.InitialPosition
                              else acc |> LazyList.head |> snd
                          let nextPos = Logic.Position.positionAfterMove x prevPos
                          Utils.prependedLaz (x, nextPos) acc
                      )
                      LazyList.empty
            |> LazyList.rev
            |> LazyList.map (fun (move, pos: Logic.Position.Position) -> (move, pos.Board))
    let movesWithNumberIndicators =
            movesWithBoards
            |> LazyList.map fst
            |> Notation.Emitter.moveTextsWithNumberIndicators areSymbolsEnabled isWhiteToMove
    let moveAnnouncements =
            movesWithNumberIndicators
            |> LazyList.filter (fun (_, b) -> not b)
            |> LazyList.map fst
            |> LazyList.map NaturalLanguage.phrase
    let boards = movesWithBoards
                 |> LazyList.map snd
                 |> LazyList.toArray
    let (whitePieces, blackPieces) =
            Notation.Emitter.textsOfPieces areSymbolsEnabled initBoard
    let announcements =
            match selectedPage with
            | OpeningPuzzlesPage ->
                let (lastMove, restRevMoves) =
                        moveAnnouncements
                        |> LazyList.rev
                        |> LazyList.uncons
                let firstAnnouncements = LazyList.ofList [ "first moves" ]
                let middleAnnouncements = LazyList.rev restRevMoves
                let lastAnnouncements = LazyList.ofList [ "best move"; lastMove ]
                [ firstAnnouncements; middleAnnouncements; lastAnnouncements ]
            | _ ->
                let (firstAnnouncements, secondAnnouncements, thirdAnnouncements) =
                        let whitePiecesAnnouncements =
                                whitePieces
                                |> LazyList.map NaturalLanguage.phrase
                                |> Utils.prependedLaz "white pieces" 
                        let blackPiecesAnnouncements =
                                blackPieces
                                |> LazyList.map NaturalLanguage.phrase
                                |> Utils.prependedLaz "black pieces"
                        if isWhiteToMove
                            then (
                                    LazyList.ofList [ "white to play" ],
                                    whitePiecesAnnouncements,
                                    blackPiecesAnnouncements
                                 )
                            else (
                                LazyList.ofList [ "black to play" ],
                                blackPiecesAnnouncements,
                                whitePiecesAnnouncements
                             )
                let lastAnnouncements =
                        moveAnnouncements
                        |> LazyList.take 1
                        |> Utils.prependedLaz "best move"
                [ firstAnnouncements; secondAnnouncements; thirdAnnouncements; lastAnnouncements ]
            |> LazyList.ofList
            |> LazyList.concat
            |> LazyList.toArray
    { CategoryId = categoryId
      Level = level
      IndexInLevel = indexInLevel
      IsWhiteToMove = isWhiteToMove
      InitBoard = initBoard
      MovesWithNumberIndicators = movesWithNumberIndicators
      Boards = boards
      WhitePieces = whitePieces
      BlackPieces = blackPieces
      Announcements  = announcements }

let resetConfigOptions(): unit =
    Preferences.removeIfExists Preferences.areCoordsEnabledKey
    Preferences.removeIfExists Preferences.areSymbolsEnabledKey
    Preferences.removeIfExists Preferences.fontSizeKey
    Preferences.removeIfExists Preferences.selectedLocaleKey
    Preferences.removeIfExists Preferences.speechPitchKey

let initConfigOptions(): ConfigOptions =
    { AreCoordsEnabled = Preferences.areCoordsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreCoordsEnabled
      BoardSize = Preferences.boardSizeKey |> Preferences.tryGetFloat |> Option.defaultValue defaultBoardSize
      AreSymbolsEnabled = Preferences.areSymbolsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreSymbolsEnabled
      FontSize = Preferences.fontSizeKey |> Preferences.tryGetFloat |> Option.defaultValue defaultFontSize
      SelectedLocale = Preferences.selectedLocaleKey |> Preferences.tryGetInt
      SpeechPitch = Preferences.speechPitchKey |> Preferences.tryGetFloat |> Option.defaultValue defaultSpeechPitch }

let init(): Model =
    let initCfgOpts = initConfigOptions()
    let endgameJsonStr = Preferences.endgameJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultEndgameJsonStr
    { SelectedPage = IntroPage
      Locales = LazyList.empty
      IsDisplayBoardOptionEnabled = Preferences.isDisplayBoardOptionEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultIsDisplayBoardOptionEnabled
      ConfigOptions = initCfgOpts
      EndgameJsonStr = Preferences.endgameJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultEndgameJsonStr
      OpeningJsonStr = Preferences.openingJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultOpeningJsonStr
      CurrentGame = endgameJsonStr |> Notation.Parser.jsonOfGame |> mechanicToCurrentGame initCfgOpts.AreSymbolsEnabled IntroPage
      CurrentMoveIndex = None
      IsPuzzleSolved = false
      CurrentAnnouncementIndex = 0
      DidVolumeNoteClicked = Preferences.didVolumeNoteClickedKey |> Preferences.tryGetBool |> Option.defaultValue defaultDidVolumeNoteClicked }
