module BlindfoldChessMechanics.Logic.Game

open System.Text.Json
open System.Text.Json.Serialization

open BlindfoldChessMechanics.Logic

type NotedResult = White | Black | Draw

[<JsonFSharpConverter>]
type Game = { MetaTags: Map<string,string>
              InitialPosition: Position.Position
              Moves: Position.Move array
              Result: NotedResult option }

let jsonOptions = JsonSerializerOptions()
jsonOptions.Converters.Add(JsonFSharpConverter())
