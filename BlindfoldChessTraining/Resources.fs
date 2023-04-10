module BlindfoldChessTraining.Resources

open FSharpx.Collections
open System.Reflection
open System.IO
open Xamarin.Forms

let image name =
    $"BlindfoldChessTraining.resources.images.%s{name}.png"
    |> ImageSource.FromResource

let lines resourceName =
    seq {
        use st = Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
        use sr = new StreamReader(st)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }
    |> LazyList.ofSeq
