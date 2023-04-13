module BlindfoldChessTraining.Resources

open FSharpx.Collections
open Microsoft.Maui.Storage
open System.IO

let puzzleLines resourceName =
    async {
        let! stream = FileSystem.OpenAppPackageFileAsync(resourceName) |> Async.AwaitTask
        let reader = new StreamReader(stream)
        return reader.ReadToEnd().Split()
    }
    |> Async.RunSynchronously
    |> LazyList.ofSeq
    |> LazyList.filter (fun s -> s.Trim() <> "")

let endgamePuzzleLines () = puzzleLines "endgame_puzzles.jsonl"
let openingPuzzleLines () = puzzleLines "opening_puzzles.jsonl"
