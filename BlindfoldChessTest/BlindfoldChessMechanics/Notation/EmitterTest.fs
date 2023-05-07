module BlindfoldChessTest.BlindfoldChessMechanics.Notation.EmitterTest

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics.Logic.Board
open BlindfoldChessMechanics.Logic.Position
open BlindfoldChessMechanics.Notation.Emitter
open BlindfoldChessTest
open BlindfoldChessTest.BlindfoldChessMechanics.Logic
open FSharpx.Collections
open NUnit.Framework

[<Test>]
let ``Texts of pieces of initial position without figures`` () =
    let whitePieces, blackPieces = textsOfPieces false Board.init
    let whitePiecesList = LazyList.toList whitePieces
    let blackPiecesList = LazyList.toList blackPieces
    let act = (whitePiecesList, blackPiecesList)

    let exp =
        ([ "Ke1"
           "Pa2"
           "Pb2"
           "Pc2"
           "Pd2"
           "Pe2"
           "Pf2"
           "Pg2"
           "Ph2"
           "Nb1"
           "Ng1"
           "Bc1"
           "Bf1"
           "Ra1"
           "Rh1"
           "Qd1" ],
         [ "Ke8"
           "Pa7"
           "Pb7"
           "Pc7"
           "Pd7"
           "Pe7"
           "Pf7"
           "Pg7"
           "Ph7"
           "Nb8"
           "Ng8"
           "Bc8"
           "Bf8"
           "Ra8"
           "Rh8"
           "Qd8" ])

    Assert.AreEqual(exp, act)

[<Test>]
let ``Texts of pieces of initial position with figures`` () =
    let whitePieces, blackPieces = textsOfPieces true Board.init
    let whitePiecesList = LazyList.toList whitePieces
    let blackPiecesList = LazyList.toList blackPieces
    let act = (whitePiecesList, blackPiecesList)

    let exp =
        ([ "♔e1"
           "♙a2"
           "♙b2"
           "♙c2"
           "♙d2"
           "♙e2"
           "♙f2"
           "♙g2"
           "♙h2"
           "♘b1"
           "♘g1"
           "♗c1"
           "♗f1"
           "♖a1"
           "♖h1"
           "♕d1" ],
         [ "♚e8"
           "♟︎a7"
           "♟︎b7"
           "♟︎c7"
           "♟︎d7"
           "♟︎e7"
           "♟︎f7"
           "♟︎g7"
           "♟︎h7"
           "♞b8"
           "♞g8"
           "♝c8"
           "♝f8"
           "♜a8"
           "♜h8"
           "♛d8" ])

    Assert.AreEqual(exp, act)

[<Test>]
let ``O-O move text`` () =
    let act =
        { Piece = King
          FromCoords = (0, 4)
          ToCoords = (0, 6)
          IsCapture = false
          Promotion = None
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [||] }
        |> moveText true false

    Assert.AreEqual("O-O", act)

[<Test>]
let ``O-O-O move text`` () =
    let act =
        { Piece = King
          FromCoords = (7, 4)
          ToCoords = (7, 2)
          IsCapture = false
          Promotion = None
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [||] }
        |> moveText false true

    Assert.AreEqual("O-O-O", act)

[<Test>]
let ``bxc3 move text`` () =
    let act =
        { Piece = Pawn
          FromCoords = (1, 3)
          ToCoords = (2, 2)
          IsCapture = true
          Promotion = None
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [| (1, 1) |] }
        |> moveText true false

    Assert.AreEqual("dxc3", act)

[<Test>]
let ``gxh1=♜# move text`` () =
    let act =
        { Piece = Pawn
          FromCoords = (1, 6)
          ToCoords = (0, 7)
          IsCapture = true
          Promotion = Some Rook
          IsCheck = true
          IsMate = true
          IsStalemate = false
          SamePieceCoords = [||] }
        |> moveText false true

    Assert.AreEqual("gxh1=♜#", act)

[<Test>]
let ``♞c6 move text`` () =
    let act =
        { Piece = Knight
          FromCoords = (7, 1)
          ToCoords = (5, 2)
          IsCapture = false
          Promotion = None
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [||] }
        |> moveText false true

    Assert.AreEqual("♞c6", act)

[<Test>]
let ``N1b3+ move text`` () =
    let act =
        { Piece = Knight
          FromCoords = (0, 0)
          ToCoords = (2, 1)
          IsCapture = false
          Promotion = None
          IsCheck = true
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [| (4, 0) |] }
        |> moveText false false

    Assert.AreEqual("N1b3+", act)

[<Test>]
let ``♕axh2 move text`` () =
    let act =
        { Piece = Queen
          FromCoords = (1, 0)
          ToCoords = (1, 7)
          IsCapture = true
          Promotion = None
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [| (7, 7) |] }
        |> moveText true true

    Assert.AreEqual("♕axh2", act)

[<Test>]
let ``b8=Q move text`` () =
    let act =
        { Piece = Pawn
          FromCoords = (6, 1)
          ToCoords = (7, 1)
          IsCapture = false
          Promotion = Some Queen
          IsCheck = false
          IsMate = false
          IsStalemate = false
          SamePieceCoords = [| (6, 2) |] }
        |> moveText true false

    Assert.AreEqual("b8=Q", act)

[<Test>]
let ``Multiple moves text with figures`` () =
    let act =
        [ PositionTest.forthHalfMove
          PositionTest.fifthHalfMove
          PositionTest.sixthHalfMove
          PositionTest.seventhHalfMove
          PositionTest.eighthHalfMove
          PositionTest.ninthHalfMove ]
        |> LazyList.ofList
        |> multipleMovesText true false

    let exp = "1...♞f6 2.♗e2 e6 3.h4 ♝a3 4.bxa3"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Multiple moves text with figures in king's pawn game`` () =
    let act =
        [ { Piece = Pawn
            FromCoords = (1, 4)
            ToCoords = (3, 4)
            IsCapture = false
            Promotion = None
            IsCheck = false
            IsMate = false
            IsStalemate = false
            SamePieceCoords = [||] }
          { Piece = Pawn
            FromCoords = (6, 4)
            ToCoords = (4, 4)
            IsCapture = false
            Promotion = None
            IsCheck = false
            IsMate = false
            IsStalemate = false
            SamePieceCoords = [||] }
          { Piece = Knight
            FromCoords = (0, 6)
            ToCoords = (2, 5)
            IsCapture = false
            Promotion = None
            IsCheck = false
            IsMate = false
            IsStalemate = false
            SamePieceCoords = [||] }
          { Piece = Knight
            FromCoords = (7, 1)
            ToCoords = (5, 2)
            IsCapture = false
            Promotion = None
            IsCheck = false
            IsMate = false
            IsStalemate = false
            SamePieceCoords = [||] } ]
        |> LazyList.ofList
        |> multipleMovesText true true

    let exp = "1.e4 e5 2.♘f3 ♞c6"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text of initial one`` () =
    let act = init |> positionText
    let exp = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after first half movement`` () =
    let act = PositionTest.positionAfterFirstHalfMovement |> positionText
    let exp = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after second half movement`` () =
    let act = PositionTest.positionAfterSecondHalfMovement |> positionText
    let exp = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after third half movement`` () =
    let act = PositionTest.positionAfterThirdHalfMovement |> positionText
    let exp = "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after forth half move`` () =
    let act = PositionTest.positionAfterForthHalfMove |> positionText
    let exp = "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 3"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after fifth half move`` () =
    let act = PositionTest.positionAfterFifthHalfMove |> positionText
    let exp = "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPPBPPP/RNBQK1NR b KQkq - 2 3"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after sixth half move`` () =
    let act = PositionTest.positionAfterSixthHalfMove |> positionText
    let exp = "rnbqkb1r/ppp2ppp/4pn2/3P4/8/8/PPPPBPPP/RNBQK1NR w KQkq - 0 4"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after seventh half move`` () =
    let act = PositionTest.positionAfterSeventhHalfMove |> positionText
    let exp = "rnbqkb1r/ppp2ppp/4pn2/3P4/7P/8/PPPPBPP1/RNBQK1NR b KQkq h3 0 4"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after eighth half move`` () =
    let act = PositionTest.positionAfterEighthHalfMove |> positionText
    let exp = "rnbqk2r/ppp2ppp/4pn2/3P4/7P/b7/PPPPBPP1/RNBQK1NR w KQkq - 1 5"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after ninth half move`` () =
    let act = PositionTest.positionAfterNinthHalfMove |> positionText
    let exp = "rnbqk2r/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR b KQkq - 0 5"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after tenth half move`` () =
    let act = PositionTest.positionAfterTentHalfMove |> positionText
    let exp = "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR w KQ - 1 6"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after eleventh half move`` () =
    let act = PositionTest.positionAfterEleventhHalfMove |> positionText
    let exp = "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQ1KNR b - - 2 6"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after twelfth half move`` () =
    let act = PositionTest.positionAfterTwelfthHalfMove |> positionText
    let exp = "rnbq1rk1/pp3ppp/4pn2/2pP4/7P/P7/P1PPBPP1/RNBQ1KNR w - c6 0 7"
    Assert.AreEqual(exp, act)

[<Test>]
let ``Position text after thirteenth half move`` () =
    let act = PositionTest.positionAfterThirteenthHalfMove |> positionText
    let exp = "rnbq1rk1/pp3ppp/2P1pn2/8/7P/P7/P1PPBPP1/RNBQ1KNR b - - 0 7"
    Assert.AreEqual(exp, act)

let exampleGame =
    { Game.MetaTags = Map.ofArray [| ("White", "me"); ("Black", "you") |]
      Game.InitialPosition = PositionTest.positionAfterThirdHalfMovement
      Game.Moves =
        [| PositionTest.forthHalfMove
           PositionTest.fifthHalfMove
           PositionTest.sixthHalfMove
           PositionTest.seventhHalfMove
           PositionTest.eighthHalfMove
           PositionTest.ninthHalfMove
           PositionTest.tenthHalfMove
           PositionTest.eleventhHalfMove
           PositionTest.twelfthHalfMove
           PositionTest.thirteenthHalfMove |]
      Game.Result = Some Game.Draw }

[<Test>]
let ``Game text`` () =
    let act = exampleGame |> gameText

    let exp =
        String.concat
            "\n"
            [| """[FEN "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"]"""
               """[Black "you"]"""
               """[White "me"]"""
               ""
               "1...Nf6 2.Be2 e6 3.h4 Ba3 4.bxa3 O-O 5.Kf1 c5 6.dxc6  1/2-1/2" |]

    Assert.AreEqual(exp, act)

[<Test>]
let ``Game json`` () =
    let act = exampleGame |> gameJson
    Assert.AreEqual(ResourcesAsCode.exampleGameJson, act)
