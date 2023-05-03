module BlindfoldChessTraining.Speech

open FSharpx.Collections
open Microsoft.Maui.Media

let safeLocaleIndex locales localeIndex =
    if localeIndex <= LazyList.length locales then
        localeIndex
    else
        0

let loadLocales () =
    async {
        let! locales = TextToSpeech.GetLocalesAsync() |> Async.AwaitTask

        let validLocales =
            locales
            |> Seq.filter (fun loc -> loc.Name <> null && loc.Name.Trim() <> "")
            |> Seq.sortBy (fun loc -> loc.Name)
            |> LazyList.ofSeq

        return validLocales
    }

let localeNamesPreDefaulted locales =
    locales
    |> LazyList.map (fun (loc: Locale) -> loc.Name)
    |> LazyList.cons "Default"
    |> LazyList.toList

let speak pitch locales localeIndex text =
    let pitch = System.Nullable pitch

    let i = safeLocaleIndex locales localeIndex

    let settings =
        match i with
        | 0 -> SpeechOptions(Pitch = pitch)
        | v -> SpeechOptions(Pitch = pitch, Locale = Seq.item (v - 1) locales)

    TextToSpeech.SpeakAsync(text, settings) |> Async.AwaitTask
