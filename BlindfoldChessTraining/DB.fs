module BlindfoldChessTraining.DB

open System
open System.IO
open SQLite

open BlindfoldChessMechanics
open FSharpx.Collections
open System.Reflection

type Puzzle = { Category_id: int
                Index_in_level: int
                Level: int
                Game : string }

let g = { Category_id = 0
          Index_in_level = 0
          Level = 0
          Game = "" }

let connection: SQLiteConnection =
    let folderPath: string =
        Environment.SpecialFolder.LocalApplicationData
        |> Environment.GetFolderPath
    let path: string = Path.Combine(folderPath, "BlindfoldChessTraining.db3")
    new SQLiteConnection(path)

let doesTableExist(): bool =
    let q = "SELECT name FROM sqlite_master WHERE type='table' AND name='puzzle'"
    connection.Query(q).Count > 0

let createTable(): unit =
    """
    CREATE TABLE IF NOT EXISTS puzzle (
       category_id INTEGER NOT NULL,
       level INTEGER NOT NULL,
       index_in_level INTEGER NOT NULL,
       game TEXT NOT NULL,
       PRIMARY KEY (category_id, level, index_in_level)
    )
    """
    |> connection.Execute
    |> ignore

let insertEndgamePuzzles(): string =
    "BlindfoldChessTraining.resources.puzzles.endgame_puzzles.jsonl"
    |> Resources.lines
    |> LazyList.length
    |> string
