module BlindfoldChessTraining.Resources

open FSharp.Data
open FSharpx.Collections
open Microsoft.Maui.Storage
open System.IO
open Types

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

let sponsorDetails () =
    let url =
        "https://mrdimosthenis.github.io/BlindfoldChessTraining/sponsor.json"

    try
        Http.RequestString(url, timeout = 3000)
        |> Newtonsoft.Json.JsonConvert.DeserializeObject<Sponsor>
        |> Some
    with _ ->
        None
