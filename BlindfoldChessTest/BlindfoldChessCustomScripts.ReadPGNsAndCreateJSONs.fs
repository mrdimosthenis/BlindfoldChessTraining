module BlindfoldChessTest.BlindfoldChessCustomScripts.ReadPGNsAndCreateJSONs

//open Xunit
//open FsUnit.Xunit
//open FSharpx.Collections
//
//open BlindfoldChessMechanics
//open BlindfoldChessMechanics.Logic
//open BlindfoldChessMechanics.Notation
//
//[<Fact>]
//let ``Convert endgame puzzles to v300`` () =
//    @"C:\Users\MrDIM\Desktop\puzzles\endgame_puzzles.pgn"
//    |> Parser.fileOfGameTexts
//    |> Utils.lazIndexed
//    |> LazyList.map
//            (fun (i, game) ->
//                let newInitPosition = Position.positionAfterMove game.Moves.[0] game.InitialPosition
//                let newMoves = [|game.Moves.[1]|]
//                let newMetaTags = [ ("category_id", "0")
//                                    ("level", string (i / 50))
//                                    ("index_in_level", string (i % 50)) ]
//                                  |> Map.ofList
//                { Game.MetaTags = newMetaTags
//                  Game.InitialPosition = newInitPosition
//                  Game.Moves = newMoves
//                  Game.Result = game.Result }
//            )
//    |> Emitter.gameFileJsons "C:\Users\MrDIM\Desktop\endgame_puzzles_v310.jsonl"
//    |> should equal ()
//
//[<Fact>]
//let ``Convert opening puzzles to v300`` () =
//    @"C:\Users\MrDIM\Desktop\puzzles\fixed_opening_puzzles.pgn"
//    |> Parser.fileOfGameTexts
//    |> Utils.lazIndexed
//    |> LazyList.map
//            (fun (i, game) ->
//                let newMetaTags = [ ("category_id", "1")
//                                    ("level", string (i / 50))
//                                    ("index_in_level", string (i % 50)) ]
//                                  |> Map.ofList
//                { game with MetaTags = newMetaTags }
//            )
//    |> Emitter.gameFileJsons "C:\Users\MrDIM\Desktop\opening_puzzles_v310.jsonl"
//    |> should equal ()
//