module BlindfoldChessTraining.Speech

open FSharpx.Collections
open Microsoft.Maui.Media

let loadLocales () =
    async {
        let! locales = TextToSpeech.GetLocalesAsync() |> Async.AwaitTask
        let validLocales = locales
                           |> Seq.filter (fun loc -> loc.Name <> null && loc.Name.Trim() <> "")
                           |> Seq.sortBy (fun loc -> loc.Name)
                           |> LazyList.ofSeq
        return validLocales
    }

let localeNames locales =
    LazyList.map (fun (loc: Locale) -> loc.Name) locales

let speak pitch locales localesIndex text =
    let pitch = System.Nullable pitch

    let settings =
        match localesIndex with
        | Some i when i < LazyList.length locales -> SpeechOptions(Pitch = pitch, Locale = Seq.item i locales)
        | _ -> SpeechOptions(Pitch = pitch)

    TextToSpeech.SpeakAsync(text, settings) |> Async.AwaitTask
