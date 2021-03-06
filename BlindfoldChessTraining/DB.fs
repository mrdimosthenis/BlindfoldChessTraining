﻿module BlindfoldChessTraining.DB

open System
open System.IO
open SQLite

open BlindfoldChessMechanics
open FSharpx.Collections

// types

//TODO: change it on version updates
type PuzzleObject_V_3_1_0() =
    member val CategoryId: int = 0 with get, set
    member val Level: int = 0 with get, set
    member val IndexInLevel: int = 0 with get, set
    member val Game: string = "" with get, set

// constants

//TODO: change it on version updates
let tableName: string = "puzzleobject_v_3_1_0"

let connection: SQLiteConnection =
    let folderPath: string =
        Environment.SpecialFolder.LocalApplicationData
        |> Environment.GetFolderPath
    let path: string = Path.Combine(folderPath, "BlindfoldChessTraining.db3")
    new SQLiteConnection(path)

let indexedColumns : string array = [| "categoryid"; "level"; "indexinlevel" |]

// bd functions

let doesTableExist(): bool =
    connection.GetTableInfo(tableName).Count > 0

let createTable(): unit =
    connection.CreateTable<PuzzleObject_V_3_1_0>() |> ignore
    connection.CreateIndex(tableName, indexedColumns, true) |> ignore

let insertPuzzles(resourceName: string): unit =
    resourceName
    |> Resources.lines
    |> LazyList.filter (fun s -> s.Trim() <> "")
    |> LazyList.map
            ( fun s ->
                let game = Notation.Parser.jsonOfGame s
                let obj = PuzzleObject_V_3_1_0()
                obj.CategoryId <- game.MetaTags.Item("category_id") |> int
                obj.Level <- game.MetaTags.Item("level") |> int
                obj.IndexInLevel <- game.MetaTags.Item("index_in_level") |> int
                obj.Game <- s
                obj
            )
    |> connection.InsertAll
    |> ignore

let getGameJsonStr(categoryId: int, level: int, indexInLevel: int): string =
    connection
        .Table<PuzzleObject_V_3_1_0>()
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
