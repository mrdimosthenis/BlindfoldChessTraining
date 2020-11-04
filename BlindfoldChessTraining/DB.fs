module BlindfoldChessTraining.DB

open System
open System.IO
open SQLite

open BlindfoldChessMechanics
open FSharpx.Collections

// types

type Puzzle = { CategoryId: int
                IndexInLevel: int
                Level: int
                Game : string }

type PuzzleObject() =
    member val CategoryId: int = 0 with get, set
    member val IndexInLevel: int = 0 with get, set
    member val Level: int = 0 with get, set
    member val Game: string = "" with get, set

// connection

let connection: SQLiteConnection =
    let folderPath: string =
        Environment.SpecialFolder.LocalApplicationData
        |> Environment.GetFolderPath
    let path: string = Path.Combine(folderPath, "BlindfoldChessTraining.db3")
    new SQLiteConnection(path)

// conversions

let puzzleToObj (puzzle: Puzzle): PuzzleObject =
    let obj = PuzzleObject()
    obj.CategoryId <- puzzle.CategoryId
    obj.IndexInLevel <- puzzle.IndexInLevel
    obj.Level <- puzzle.Level
    obj.Game <- puzzle.Game
    obj

let puzzleOfObj (obj: PuzzleObject) : Puzzle =
    { CategoryId = obj.CategoryId
      IndexInLevel = obj.IndexInLevel
      Level = obj.Level
      Game  = obj.Game }

// bd functions

let doesTableExist(): bool =
    connection.GetTableInfo("puzzleobject").Count > 0

let createTable(): unit =
    connection.CreateTable<PuzzleObject>() |> ignore
    connection.CreateIndex("puzzleobject", [| "categoryid"; "level"; "indexinlevel" |], true) |> ignore

let insertPuzzles(resourceName: string): unit =
    resourceName
    |> Resources.lines
    |> LazyList.filter (fun s -> s.Trim() <> "")
    |> LazyList.map
            ( fun s ->
                let game = Notation.Parser.jsonOfGame s
                puzzleToObj { CategoryId = game.MetaTags.Item("category_id") |> int
                              IndexInLevel = game.MetaTags.Item("level") |> int
                              Level = game.MetaTags.Item("index_in_level") |> int
                              Game = s }
            )
    |> connection.InsertAll
    |> ignore

let getGameJsonStr(categoryId: int, level: int, indexInLevel: int): string =
    connection
        .Table<PuzzleObject>()
        .Where(fun obj -> obj.CategoryId = categoryId && obj.Level = level && obj.IndexInLevel = indexInLevel)
        .First()
        .Game

// executable statement

if doesTableExist()
    then ()
else connection.BeginTransaction()
     createTable()
     insertPuzzles("BlindfoldChessTraining.resources.puzzles.endgame_puzzles.jsonl")
     insertPuzzles("BlindfoldChessTraining.resources.puzzles.opening_puzzles.jsonl")
     connection.Commit()
