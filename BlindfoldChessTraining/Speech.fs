module BlindfoldChessTraining.Speech

open Xamarin.Essentials
open Fabulous

let loadLocales(): Cmd<Msg.Msg> =
    async {
        let! locales = TextToSpeech.GetLocalesAsync() |> Async.AwaitTask
        let localesMsg = locales
                         |> Seq.sortBy (fun loc -> loc.Name)
                         |> Seq.cache
                         |> Msg.LocalesLoaded
        return localesMsg
    }
    |> Cmd.ofAsyncMsg

let localeNames (locales: Locale seq): string seq =
    locales
    |> Seq.map (fun loc -> loc.Name)
    |> Seq.cache

let speak (model: Model.Model) (text: string): unit =
    let pitch = new System.Nullable<float32>(float32 model.ConfigOptions.SpeechPitch)
    let settings = match model.ConfigOptions.SelectedLocale with
                   | Some i when i < Seq.length model.Locales ->
                        new SpeechOptions(Pitch = pitch, Locale = Seq.item i model.Locales)
                   | _ ->
                        new SpeechOptions(Pitch = pitch)
    TextToSpeech.SpeakAsync(text, settings)
    |> Async.AwaitTask
    |> Async.StartImmediate
