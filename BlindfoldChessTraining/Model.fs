module BlindfoldChessTraining.Model

open Xamarin.Essentials
open FSharpx.Collections
open BlindfoldChessMechanics

// types

type SelectedPage =
    | HomePage
    | OpeningPuzzlesPage
    | EndgamePuzzlesPage
    | DescriptionPage
    | OptionsPage
    | CreditsPage

type ConfigOptions = { AreCoordsEnabled: bool
                       AreSymbolsEnabled: bool
                       FontSize: float
                       SelectedLocale: int option
                       SpeechPitch: float }

type GameWithBoards =
        { Level: int
          IndexInLevel: int
          IsWhiteToMove: bool
          InitBoard: Logic.Board.Board
          MovesWithBoards: (Logic.Position.Move * Logic.Board.Board) array }

type Model = 
    { SelectedPage: SelectedPage
      Locales: Locale LazyList
      ConfigOptions : ConfigOptions
      EndgameJsonStr: string
      OpeningJsonStr: string
      CurrentGameWithBoards: GameWithBoards
      CurrentMoveIndex: int option }

// default values

let defaultAreCoordsEnabled: bool = true
let defaultAreSymbolsEnabled: bool = false
let defaultFontSize: float = 17.0
let defaultSpeechPitch: float = 1.0

let defaultEndgameJsonStr: string = DB.getGameJsonStr(0, 0, 0)
let defaultOpeningJsonStr: string = DB.getGameJsonStr(1, 0, 0)

// functions

let gameToGameWithBoards(game: Logic.Game.Game): GameWithBoards =
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
            |> LazyList.toArray
    { Level = level
      IndexInLevel = indexInLevel
      IsWhiteToMove = isWhiteToMove
      InitBoard = initBoard
      MovesWithBoards = movesWithBoards }

let resetConfigOptions(): unit =
    Preferences.removeIfExists Preferences.areCoordsEnabledKey
    Preferences.removeIfExists Preferences.areSymbolsEnabledKey
    Preferences.removeIfExists Preferences.fontSizeKey
    Preferences.removeIfExists Preferences.selectedLocaleKey
    Preferences.removeIfExists Preferences.speechPitchKey

let initConfigOptions(): ConfigOptions =
    { AreCoordsEnabled = Preferences.areCoordsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreCoordsEnabled
      AreSymbolsEnabled = Preferences.areSymbolsEnabledKey |> Preferences.tryGetBool |> Option.defaultValue defaultAreSymbolsEnabled
      FontSize = Preferences.fontSizeKey |> Preferences.tryGetFloat |> Option.defaultValue defaultFontSize
      SelectedLocale = Preferences.selectedLocaleKey |> Preferences.tryGetInt
      SpeechPitch = Preferences.speechPitchKey |> Preferences.tryGetFloat |> Option.defaultValue defaultSpeechPitch }

let init(): Model =
    let endgameJsonStr = Preferences.endgameJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultEndgameJsonStr
    { SelectedPage = HomePage
      Locales = LazyList.empty
      ConfigOptions = initConfigOptions()
      EndgameJsonStr = Preferences.endgameJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultEndgameJsonStr
      OpeningJsonStr = Preferences.openingJsonStrKey |> Preferences.tryGetString |> Option.defaultValue defaultOpeningJsonStr
      CurrentGameWithBoards = endgameJsonStr |> Notation.Parser.jsonOfGame |> gameToGameWithBoards
      CurrentMoveIndex = None }
