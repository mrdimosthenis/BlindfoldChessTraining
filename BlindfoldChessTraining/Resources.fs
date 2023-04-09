module BlindfoldChessTraining.Resources

open FSharpx.Collections
open System.IO
open System.Reflection

let lines resourceName =
    seq {
        use st = Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
        use sr = new StreamReader(st)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }
    |> LazyList.ofSeq
