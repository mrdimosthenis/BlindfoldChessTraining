module BlindfoldChessMechanics.Logic.Game

open BlindfoldChessMechanics.Logic

type NotedResult = White | Black | Draw

type Game = { MetaTags: Map<string,string>
              InitialPosition: Position.Position
              Moves: Position.Move seq
              Result: NotedResult option }
