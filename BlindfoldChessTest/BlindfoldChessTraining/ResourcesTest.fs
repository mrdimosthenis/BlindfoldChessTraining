module BlindfoldChessTest.BlindfoldChessTraining.ResourcesTest

open BlindfoldChessMechanics
open BlindfoldChessTraining
open FSharpx.Collections
open NUnit.Framework

let endgamePuzzles =
    "BlindfoldChessTraining.resources.puzzles.endgame_puzzles.jsonl"
    |> Resources.lines
    |> LazyList.filter (fun (s: string) -> s.Trim() <> "")
    |> LazyList.toArray

let openingPuzzles =
    "BlindfoldChessTraining.resources.puzzles.opening_puzzles.jsonl"
    |> Resources.lines
    |> LazyList.filter (fun (s: string) -> s.Trim() <> "")
    |> LazyList.toArray

[<Test>]
let ``parse and emit endgame puzzles`` () =
    let act =
        endgamePuzzles
        |> LazyList.ofArray
        |> LazyList.map Notation.Parser.jsonOfGame
        |> LazyList.map Notation.Emitter.gameJson
        |> LazyList.toArray

    Assert.AreEqual(endgamePuzzles, act)

[<Test>]
let ``parse and emit opening puzzles`` () =
    let act =
        openingPuzzles
        |> LazyList.ofArray
        |> LazyList.map Notation.Parser.jsonOfGame
        |> LazyList.map Notation.Emitter.gameJson
        |> LazyList.toArray

    Assert.AreEqual(openingPuzzles, act)
