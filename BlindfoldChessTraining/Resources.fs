module BlindfoldChessTraining.Resources

open Fabulous.XamarinForms
open Xamarin.Forms
open System.Reflection
open System.IO
open FSharpx.Collections

let image (name: string): Image.Value = 
    name
    |> sprintf "BlindfoldChessTraining.resources.images.%s.png"
    |> ImageSource.FromResource
    |> Image.fromImageSource

let lines (resourceName: string): LazyList<string> =
    seq {
        use st = Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
        use sr = new StreamReader (st)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    } |> LazyList.ofSeq
