module BlindfoldChessMechanics.Notation.ParserTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Notation.Parser
open BlindfoldChessMechanics.Logic
open System.IO
open FSharpx.Collections
open BlindfoldChessMechanics

// tests

[<Fact>]
let ``Parse text of meta tags`` () =
    """[Event "Rated Blitz game"]
    [Site "https://lichess.org/H7z1mmv9"]
    [Date "2020.09.24"]
    [White "geordie"]
    [Black "DDT3000"]
    [Result "0-1"]
    [UTCDate "2020.09.24"]
    [UTCTime "22:39:57"]
    [WhiteElo "2413"]
    [BlackElo "2306"]
    [WhiteRatingDiff "-7"]
    [BlackRatingDiff "+11"]
    [WhiteTitle "NM"]
    [Variant "Standard"]
    [TimeControl "180+2"]
    [ECO "B40"]
    [Termination "Time forfeit"]
    
    1. e4 { [%eval 0.0] [%clk 0:03:00] } 1... c5 { [%eval 0.0] [%clk 0:03:00] }"""
    |> textOfMetaTags
    |> Map.toArray
    |> should equal
        [| ("Black", "DDT3000")
           ("BlackElo", "2306")
           ("BlackRatingDiff", "+11")
           ("Date", "2020.09.24")
           ("ECO", "B40")
           ("Event", "Rated Blitz game")
           ("Result", "0-1")
           ("Site", "https://lichess.org/H7z1mmv9")
           ("Termination", "Time forfeit")
           ("TimeControl", "180+2")
           ("UTCDate", "2020.09.24")
           ("UTCTime", "22:39:57")
           ("Variant", "Standard")
           ("White", "geordie")
           ("WhiteElo", "2413")
           ("WhiteRatingDiff", "-7")
           ("WhiteTitle", "NM") |]

[<Fact>]
let ``Parse text of moves with result`` () =
    let text = """[Event "Rated Blitz game"]
    [Site "https://lichess.org/H7z1mmv9"]
    [Date "2020.09.24"]
    [White "geordie"]
    [Black "DDT3000"]
    [Result "0-1"]
    [UTCDate "2020.09.24"]
    [UTCTime "22:39:57"]
    [WhiteElo "2413"]
    [BlackElo "2306"]
    [WhiteRatingDiff "-7"]
    [BlackRatingDiff "+11"]
    [WhiteTitle "NM"]
    [Variant "Standard"]
    [TimeControl "180+2"]
    [ECO "B40"]
    [Termination "Time forfeit"]
    
    1. e4 { [%eval 0.0] [%clk 0:03:00] } 1... c5 { [%eval 0.0] [%clk 0:03:00] } 
    2. Nf3 { [%eval 0.0] [%clk 0:03:01] } 2... e6 { [%eval 0.13] [%clk 0:03:01] } 
    3. d3 { [%eval 0.0] [%clk 0:03:01] } 3... Nc6 { [%eval 0.01] [%clk 0:03:01] } 
    4. g3 { [%eval 0.0] [%clk 0:03:03] } 4... g6 { [%eval 0.17] [%clk 0:03:02] } 
    5. Bg5 { [%eval 0.09] [%clk 0:03:03] } 5... Qc7 { [%eval 0.13] [%clk 0:02:54] } 
    6. Nbd2 { [%eval -0.31] [%clk 0:03:04] } 6... Bg7 { [%eval -0.33] [%clk 0:02:53] } 
    7. Bf4 { [%eval -0.52] [%clk 0:03:02] } 7... d6 { [%eval -0.29] [%clk 0:02:52] } 
    8. Nc4 { [%eval -0.23] [%clk 0:03:03] } 8... Bf8 { [%eval 0.48] [%clk 0:02:17] } 
    9. a4 { [%eval 0.43] [%clk 0:02:52] } 9... Na5 { [%eval 1.57] [%clk 0:02:06] } 
    10. e5 { [%eval 1.74] [%clk 0:02:42] } 10... Nxc4 { [%eval 1.72] [%clk 0:02:05] } 
    11. dxc4 { [%eval 1.93] [%clk 0:02:43] } 11... Qa5+ { [%eval 1.92] [%clk 0:01:55] } 
    12. c3 { [%eval 1.8] [%clk 0:02:42] } 12... dxe5 { [%eval 1.76] [%clk 0:01:36] } 
    13. Nxe5 { [%eval 1.65] [%clk 0:02:28] } 13... Bg7 { [%eval 2.11] [%clk 0:01:31] } 
    14. Qd6 { [%eval 1.78] [%clk 0:02:05] } 14... Qb6 { [%eval 1.92] [%clk 0:01:19] } 
    15. Qxb6 { [%eval 0.87] [%clk 0:01:59] } 15... axb6 { [%eval 0.88] [%clk 0:01:21] } 
    16. Bd3 { [%eval -2.49] [%clk 0:01:55] } 16... Nf6 { [%eval 0.19] [%clk 0:01:16] } 
    17. Ke2 { [%eval -1.2] [%clk 0:01:46] } 17... Nh5 { [%eval -1.26] [%clk 0:01:10] } 
    18. Be4 { [%eval -2.18] [%clk 0:01:35] } 18... g5 { [%eval -2.09] [%clk 0:01:02] } 
    19. Nxf7 { [%eval -2.12] [%clk 0:00:34] } 19... Nxf4+ { [%eval -2.24] [%clk 0:00:53] } 
    20. gxf4 { [%eval -2.39] [%clk 0:00:33] } 20... Kxf7 { [%eval -2.3] [%clk 0:00:54] } 
    21. fxg5 { [%eval -2.14] [%clk 0:00:35] } 21... Bd7 { [%eval -2.29] [%clk 0:00:42] } 
    22. b3 { [%eval -3.69] [%clk 0:00:30] } 22... Bc6 { [%eval -3.68] [%clk 0:00:42] } 
    23. Bxc6 { [%eval -3.45] [%clk 0:00:20] } 23... bxc6 { [%eval -3.32] [%clk 0:00:44] } 
    24. Rac1 { [%eval -3.66] [%clk 0:00:20] } 24... Rhd8 { [%eval -3.15] [%clk 0:00:41] } 
    25. h4 { [%eval -3.09] [%clk 0:00:22] } 25... Kg6 { [%eval -2.86] [%clk 0:00:40] } 
    26. f4 { [%eval -4.09] [%clk 0:00:22] } 26... Kf5 { [%eval -3.81] [%clk 0:00:41] } 
    27. Ke3 { [%eval -4.32] [%clk 0:00:14] } 27... Rd7 { [%eval -4.43] [%clk 0:00:41] } 
    28. Rhd1 { [%eval -4.2] [%clk 0:00:11] } 28... Rad8 { [%eval -4.29] [%clk 0:00:40] } 
    29. Rxd7 { [%eval -4.35] [%clk 0:00:11] } 29... Rxd7 { [%eval -3.92] [%clk 0:00:42] } 
    30. Ra1 { [%eval -7.48] [%clk 0:00:12] } 30... Bxc3 { [%eval -7.33] [%clk 0:00:41] } 
    31. Rf1 { [%eval -8.44] [%clk 0:00:09] } 31... Bd2+ { [%eval -8.33] [%clk 0:00:40] } 
    32. Ke2 { [%eval -8.23] [%clk 0:00:06] } 32... Bxf4 { [%eval -7.87] [%clk 0:00:41] } 
    33. a5 { [%eval -8.77] [%clk 0:00:04] } 33... bxa5 { [%eval -8.64] [%clk 0:00:42] } 
    34. Ra1 { [%eval -14.93] [%clk 0:00:06] } 34... Rd2+ { [%eval -14.76] [%clk 0:00:28] } 
    35. Ke1 { [%eval -14.07] [%clk 0:00:07] } 35... Ke4 { [%eval -12.91] [%clk 0:00:24] } 
    36. Rxa5 { [%eval -66.66] [%clk 0:00:08] } 36... Kf3 { [%eval -62.08] [%clk 0:00:21] } 
    37. Rxc5 { [%eval #-3] [%clk 0:00:08] } 37... Rc2 { [%eval #-2] [%clk 0:00:19] } 0-1
    """
    let (moves, result) = textOfMovesWithResult text
    let movesArr = LazyList.toList moves
    should equal (movesArr, result)
        (
            [ "e4"; "c5";
               "Nf3"; "e6";
               "d3"; "Nc6";
               "g3"; "g6";
               "Bg5"; "Qc7";
               "Nbd2"; "Bg7";
               "Bf4"; "d6";
               "Nc4"; "Bf8";
               "a4"; "Na5";
               "e5"; "Nxc4";
               "dxc4"; "Qa5+";
               "c3"; "dxe5";
               "Nxe5"; "Bg7";
               "Qd6"; "Qb6";
               "Qxb6"; "axb6";
               "Bd3"; "Nf6";
               "Ke2"; "Nh5";
               "Be4"; "g5";
               "Nxf7"; "Nxf4+";
               "gxf4"; "Kxf7";
               "fxg5"; "Bd7";
               "b3"; "Bc6";
               "Bxc6"; "bxc6";
               "Rac1"; "Rhd8";
               "h4"; "Kg6";
               "f4"; "Kf5";
               "Ke3"; "Rd7";
               "Rhd1"; "Rad8";
               "Rxd7"; "Rxd7";
               "Ra1"; "Bxc3";
               "Rf1"; "Bd2+";
               "Ke2"; "Bxf4";
               "a5"; "bxa5";
               "Ra1"; "Rd2+";
               "Ke1"; "Ke4";
               "Rxa5"; "Kf3";
               "Rxc5"; "Rc2" ],
            Some Game.Black
        )

[<Fact>]
let ``Fen of initial position`` () =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    |> textOfFen
    |> should equal Position.init

[<Fact>]
let ``Fen of position after first half movement`` () =
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterFirstHalfMovement

[<Fact>]
let ``Fen of position after second half movement`` () =
    "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterSecondHalfMovement

[<Fact>]
let ``Fen of position after third half movement`` () =
    "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterThirdHalfMovement

[<Fact>]
let ``Fen of position after forth half move`` () =
    "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 3"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterForthHalfMove

[<Fact>]
let ``Fen of position after fifth half move`` () =
    "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPPBPPP/RNBQK1NR b KQkq - 2 3"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterFifthHalfMove

[<Fact>]
let ``Fen of position after sixth half move`` () =
   "rnbqkb1r/ppp2ppp/4pn2/3P4/8/8/PPPPBPPP/RNBQK1NR w KQkq - 0 4"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterSixthHalfMove

[<Fact>]
let ``Fen of position after seventh half move`` () =
    "rnbqkb1r/ppp2ppp/4pn2/3P4/7P/8/PPPPBPP1/RNBQK1NR b KQkq h3 0 4"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterSeventhHalfMove

[<Fact>]
let ``Fen of position after eighth half move`` () =
    "rnbqk2r/ppp2ppp/4pn2/3P4/7P/b7/PPPPBPP1/RNBQK1NR w KQkq - 1 5"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterEighthHalfMove

[<Fact>]
let ``Fen of position after ninth half move`` () =
    "rnbqk2r/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR b KQkq - 0 5"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterNinthHalfMove

[<Fact>]
let ``Fen of position after tenth half move`` () =
    "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR w KQ - 1 6"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterTentHalfMove

[<Fact>]
let ``Fen of position after eleventh half move`` () =
    "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQ1KNR b - - 2 6"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterEleventhHalfMove

[<Fact>]
let ``Fen of position after twelfth half move`` () =
    "rnbq1rk1/pp3ppp/4pn2/2pP4/7P/P7/P1PPBPP1/RNBQ1KNR w - c6 0 7"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterTwelfthHalfMove

[<Fact>]
let ``Fen of position after thirteenth half move`` () =
    "rnbq1rk1/pp3ppp/2P1pn2/8/7P/P7/P1PPBPP1/RNBQ1KNR b - - 0 7"
    |> textOfFen
    |> should equal
        PositionTest.positionAfterThirteenthHalfMove

[<Fact>]
let ``Text of small game`` () =
    """[FEN "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"]
[Black "you"]
[White "me"]

1... Nf6 2. Be2 e6 3. h4 Ba3 4. bxa3 O-O 5. Kf1 c5 6. dxc6  1/2-1/2"""
    |> textOfGame
    |> should equal
        { Game.MetaTags = Map.ofArray
                            [| ("Black", "you")
                               ("FEN", "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")
                               ("White", "me") |]
          Game.InitialPosition = PositionTest.positionAfterThirdHalfMovement
          Game.Moves = [| PositionTest.forthHalfMove
                          PositionTest.fifthHalfMove
                          PositionTest.sixthHalfMove
                          PositionTest.seventhHalfMove
                          PositionTest.eighthHalfMove
                          PositionTest.ninthHalfMove
                          PositionTest.tenthHalfMove
                          PositionTest.eleventHalfMove
                          PositionTest.twelfthHalfMove
                          PositionTest.thirteenthHalfMove |]
          Game.Result = (Some Game.Draw) }

[<Fact>]
let ``Text of large valid game`` () =
    let largeGame = textOfGame """[Event "?"]
[Site "Dusseldorf (09)"]
[Date "1908.??.??"]
[Round "?"]
[White "Wiarda A"]
[Black "Alekhine, Alexander A"]
[Result "1-0"]
[ECO "C63u"]

1.e4 e5 2.Nf3 Nc6 3.Bb5 f5 4.Nc3 fxe4 5.Nxe4 d5 6.Nxe5 dxe4 7.Nxc6 Qg5 8.
Nd4+ c6 9.Bf1 Nf6 10.d3 Qg6 11.Be3 Be7 12.Qd2 Qf7 13.Be2 O-O 14.O-O c5 15.
Nb5 Bf5 16.f3 exd3 17.Bxd3 Rad8 18.Qe2 Bxd3 19.cxd3 a6 20.Nc3 Rfe8 21.Ne4 
Qh5 22.Qc2 b6 23.Rad1 Nd5 24.Bf2 Bd6 25.Nxd6 Rxd6 26.Rfe1 Rf8 27.Bg3 Rg6 
28.Re5 Rg5 29.Rde1 Kh8 30.Rxg5 Qxg5 31.Re5 Qd8 32.a3 Ne7 33.Qc4 Nc6 34.Rd5
Qc8 35.Rd6 Re8 36.Qf7 Rd8 37.Qe6 Qxe6 38.Rxe6 Rc8 39.Bc7 Nd4 40.Rxb6 Kg8 
41.Rb8 Rxb8 42.Bxb8 Kf7 43.Be5 Nb3 44.Kf2 g6 45.Bc3 Ke6 46.Ke3 Kd5 47.g3 
Nc1 48.f4 Ke6 49.Kd2 Nb3+ 50.Ke3 Kf5 51.h3 h5 52.d4 cxd4+ 53.Bxd4 Na5 54.
Bc3 Nb3 55.Kd3 h4 56.Kc4 Nc1 57.gxh4 Ne2 58.Be5 Nxf4 59.Bxf4 Kxf4 60.b4 
Kg3 61.a4 Kxh4 62.b5 axb5+ 63.axb5 Kxh3 64.b6 g5 65.b7 g4 66.b8=Q g3 67.
Qh8+ Kg2 68.Kd3 Kg1 69.Ke2 g2 70.Qd4+ Kh1 71.Qh4+ Kg1 1-0"""
    largeGame.Moves
    |> Array.length
    |> should equal 142

[<Fact>]
let ``Json of example game`` () =
    ResourcesAsCode.exampleGameJson
    |> jsonOfGame
    |> should equal
        EmitterTest.exampleGame

//[<Fact>]
//let ``Convert endgame puzzles to v300`` () =
//    "C:\Users\MrDIM\Desktop\old-blindfold\endgame_puzzles.pgn"
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
//    |> Emitter.gameFileJsons "C:\Users\MrDIM\Desktop\endgame_puzzles_v300.pgn"
//    |> should equal ()
