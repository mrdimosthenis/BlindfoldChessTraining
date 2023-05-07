module BlindfoldChessTraining.DB

open BlindfoldChessMechanics
open BlindfoldChessTraining.Types
open FSharpx.Collections
open Microsoft.Maui.Storage
open SQLite
open System
open System.IO

// constants

// change it on version updates
let tableName = "puzzleobject_v_4_0_0"

let connection =
    let path = Path.Combine(FileSystem.AppDataDirectory, "BlindfoldChessTraining.db3")

    let flags =
        SQLiteOpenFlags.ReadWrite
        ||| SQLiteOpenFlags.Create
        ||| SQLiteOpenFlags.SharedCache

    new SQLiteConnection(path, flags)

let indexedColumns = [| "categoryid"; "level"; "indexinlevel" |]

// bd functions

let insertPuzzles puzzleLines =
    puzzleLines
    |> LazyList.map (fun s ->
        let game = Notation.Parser.jsonOfGame s
        let obj = PuzzleObject_V_4_0_0()
        obj.CategoryId <- game.MetaTags.Item("category_id") |> int
        obj.Level <- game.MetaTags.Item("level") |> int
        obj.IndexInLevel <- game.MetaTags.Item("index_in_level") |> int
        obj.Game <- s
        obj)
    |> connection.InsertAll
    |> ignore

let doesTableExist () = connection.GetTableInfo(tableName).Count > 0

let createTable () =
    connection.CreateTable<PuzzleObject_V_4_0_0>() |> ignore
    connection.CreateIndex(tableName, indexedColumns, true) |> ignore

let init () =
    connection.BeginTransaction()
    createTable ()
    insertPuzzles (Resources.endgamePuzzleLines ())
    insertPuzzles (Resources.openingPuzzleLines ())
    connection.Commit()

let getGameJsonStr categoryId level indexInLevel =
    connection
        .Table<PuzzleObject_V_4_0_0>()
        .Where(fun obj ->
            obj.CategoryId = categoryId
            && obj.Level = level
            && obj.IndexInLevel = indexInLevel)
        .First()
        .Game

let currentGame areSymbolsEnabled categoryId level indexInLevel =
    let game =
        getGameJsonStr categoryId level indexInLevel |> Notation.Parser.jsonOfGame

    let isWhiteToMove = game.InitialPosition.IsWhiteToMove
    let initBoard = game.InitialPosition.Board

    let movesWithBoards =
        game.Moves
        |> LazyList.ofArray
        |> LazyList.fold
            (fun acc x ->
                let prevPos =
                    if LazyList.isEmpty acc then
                        game.InitialPosition
                    else
                        acc |> LazyList.head |> snd

                let nextPos = Logic.Position.positionAfterMove x prevPos
                Utils.prependedLaz (x, nextPos) acc)
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

    let boards = movesWithBoards |> LazyList.map snd |> LazyList.toArray

    let whitePieces, blackPieces =
        Notation.Emitter.textsOfPieces areSymbolsEnabled initBoard

    let announcements =
        match categoryId with
        | 1 ->
            let lastMove, restRevMoves = moveAnnouncements |> LazyList.rev |> LazyList.uncons
            let firstAnnouncements = LazyList.ofList [ "first moves" ]
            let middleAnnouncements = LazyList.rev restRevMoves
            let lastAnnouncements = LazyList.ofList [ "best move"; lastMove ]
            [ firstAnnouncements; middleAnnouncements; lastAnnouncements ]
        | 0 ->
            let firstAnnouncements, secondAnnouncements, thirdAnnouncements =
                let whitePiecesAnnouncements =
                    whitePieces
                    |> LazyList.map NaturalLanguage.phrase
                    |> Utils.prependedLaz "white pieces"

                let blackPiecesAnnouncements =
                    blackPieces
                    |> LazyList.map NaturalLanguage.phrase
                    |> Utils.prependedLaz "black pieces"

                if isWhiteToMove then
                    (LazyList.ofList [ "white to play" ], whitePiecesAnnouncements, blackPiecesAnnouncements)
                else
                    (LazyList.ofList [ "black to play" ], blackPiecesAnnouncements, whitePiecesAnnouncements)

            let lastAnnouncements =
                moveAnnouncements |> LazyList.take 1 |> Utils.prependedLaz "best move"

            [ firstAnnouncements
              secondAnnouncements
              thirdAnnouncements
              lastAnnouncements ]
        | _ -> raise WrongCategoryId
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
      Announcements = announcements }
