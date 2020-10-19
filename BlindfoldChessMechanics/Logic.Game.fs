module BlindfoldChessMechanics.Logic.Game

open BlindfoldChessMechanics.Logic
open FSharpx.Collections

type NotedResult = White | Black | Draw

type Game = { MetaTags: Map<string,string>
              InitialPosition: Position.Position
              Moves: LazyList<Position.Move>
              Result: NotedResult option }
