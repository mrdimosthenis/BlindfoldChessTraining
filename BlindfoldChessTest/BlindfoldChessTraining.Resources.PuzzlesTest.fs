module BlindfoldChessTraining.Resources.PuzzlesTest

open Xunit
open FsUnit.Xunit

open FSharpx.Collections

open BlindfoldChessTraining
open BlindfoldChessMechanics

let endgamePuzzles =
    "BlindfoldChessTraining.resources.puzzles.endgame_puzzles.jsonl"
    |> Resources.lines
    |> LazyList.filter (fun s -> s.Trim() <> "")
    |> LazyList.toArray

let openingPuzzles =
    "BlindfoldChessTraining.resources.puzzles.opening_puzzles.jsonl"
    |> Resources.lines
    |> LazyList.filter (fun s -> s.Trim() <> "")
    |> LazyList.toArray

[<Fact>]
let ``parse and emit endgame puzzles`` () =
    endgamePuzzles
    |> LazyList.ofArray
    |> LazyList.map Notation.Parser.jsonOfGame
    |> LazyList.map Notation.Emitter.gameJson
    |> LazyList.toArray
    |> should equal
                endgamePuzzles

[<Fact>]
let ``parse and emit opening puzzles`` () =
    openingPuzzles
    |> LazyList.ofArray
    |> LazyList.map Notation.Parser.jsonOfGame
    |> LazyList.map Notation.Emitter.gameJson
    |> LazyList.toArray
    |> should equal
                openingPuzzles
