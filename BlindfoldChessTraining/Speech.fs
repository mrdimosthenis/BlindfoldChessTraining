module BlindfoldChessTraining.Speech

open Xamarin.Essentials

let loadLocales(): Async<Locale seq> =
    async {
        let! locales = TextToSpeech.GetLocalesAsync() |> Async.AwaitTask
        let validLocales = locales
                           |> Seq.filter (fun loc -> loc.Name <> null && loc.Name.Trim() <> "")
                           |> Seq.sortBy (fun loc -> loc.Name)
                           |> Seq.cache
        return validLocales
    }

let localeNames (locales: Locale seq): string seq =
    locales
    |> Seq.map (fun loc -> loc.Name)
    |> Seq.cache

let speak (pitch: float) (locales: Locale seq) (localesIndex: int option) (text: string): unit =
    let pitch = new System.Nullable<float32>(float32 pitch)
    let settings = match localesIndex with
                   | Some i when i < Seq.length locales ->
                        new SpeechOptions(Pitch = pitch, Locale = Seq.item i locales)
                   | _ ->
                        new SpeechOptions(Pitch = pitch)
    TextToSpeech.SpeakAsync(text, settings)
    |> Async.AwaitTask
    |> Async.StartImmediate
