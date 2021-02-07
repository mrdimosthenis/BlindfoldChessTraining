module BlindfoldChessTraining.Speech

open Xamarin.Essentials
open FSharpx.Collections
open BlindfoldChessMechanics

let loadLocales(): Async<Locale LazyList> =
    async {
        let! locales = TextToSpeech.GetLocalesAsync() |> Async.AwaitTask
        let validLocales = locales
                           |> Seq.filter (fun loc -> loc.Name <> null && loc.Name.Trim() <> "")
                           |> Seq.sortBy (fun loc -> loc.Name)
                           |> LazyList.ofSeq
        return validLocales
    }

let localeNames (locales: Locale LazyList): string LazyList =
    LazyList.map (fun (loc: Locale) -> loc.Name) locales

let speak (pitch: float) (locales: Locale LazyList) (localesIndex: int option) (text: string): Async<unit> =
    let pitch = new System.Nullable<float32>(float32 pitch)
    let settings = match localesIndex with
                   | Some i when i < LazyList.length locales ->
                        new SpeechOptions(Pitch = pitch, Locale = Seq.item i locales)
                   | _ ->
                        new SpeechOptions(Pitch = pitch)
    TextToSpeech.SpeakAsync(text, settings)
    |> Async.AwaitTask
