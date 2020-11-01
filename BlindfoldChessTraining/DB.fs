module BlindfoldChessTraining.DB

open System
open System.IO
open SQLite

open BlindfoldChessMechanics.Notation
open FSharpx.Collections

type Puzzle = { Category_id: int
                Index_in_level: int
                Level: int
                Game : string }

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
    CREATE TABLE puzzle (
       category_id INTEGER NOT NULL,
       level INTEGER NOT NULL,
       index_in_level INTEGER NOT NULL,
       game TEXT NOT NULL,
       PRIMARY KEY (category_id, level, index_in_level)
    )
    """
    |> connection.Execute
    |> ignore

let insertPuzzles(resourceName: string): unit =
    resourceName
    |> Resources.lines
    |> LazyList.filter (fun s -> s.Trim() <> "")
    |> LazyList.iter
            ( fun s ->
                let game = Parser.jsonOfGame s
                let puzzle = { Category_id = game.MetaTags.Item("category_id") |> int
                               Index_in_level = game.MetaTags.Item("level") |> int
                               Level = game.MetaTags.Item("index_in_level") |> int
                               Game = s }
                connection.Execute(
                    "INSERT INTO puzzle VALUES (?, ?, ?, ?)",
                    puzzle.Category_id, puzzle.Level, puzzle.Index_in_level, puzzle.Game
                ) |> ignore
            )

if doesTableExist()
    then ()
else connection.BeginTransaction()
     createTable()
     insertPuzzles("BlindfoldChessTraining.resources.puzzles.endgame_puzzles.jsonl")
     insertPuzzles("BlindfoldChessTraining.resources.puzzles.opening_puzzles.jsonl")
     connection.Commit()
