module BlindfoldChessMechanics.Notation.EmitterTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Notation.Emitter
open BlindfoldChessMechanics.Logic.Board
open BlindfoldChessMechanics.Logic.Position
open BlindfoldChessMechanics.Logic

[<Fact>]
let ``O-O move text`` () =
    { Piece = King
      FromCoords = (0, 4)
      ToCoords = (0, 6)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }    
    |> moveText true false
    |> should equal
        "O-O"

[<Fact>]
let ``O-O-O move text`` () =
    { Piece = King
      FromCoords = (7, 4)
      ToCoords = (7, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }    
    |> moveText false true
    |> should equal
        "O-O-O"

[<Fact>]
let ``bxc3 move text`` () =
    { Piece = Pawn
      FromCoords = (1, 3)
      ToCoords = (2, 2)
      IsCapture = true
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (1, 1) }    
    |> moveText true false
    |> should equal
        "dxc3"

[<Fact>]
let ``gxh1=♜# move text`` () =
    { Piece = Pawn
      FromCoords = (1, 6)
      ToCoords = (0, 7)
      IsCapture = true
      Promotion = Some Rook
      IsCheck = true
      IsMate = true
      IsStalemate = false
      SamePieceCoords = None }    
    |> moveText false true
    |> should equal
        "gxh1=♜#"

[<Fact>]
let ``N1b3+ move text`` () =
    { Piece = Knight
      FromCoords = (0, 0)
      ToCoords = (2, 1)
      IsCapture = false
      Promotion = None
      IsCheck = true
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (4, 0) }    
    |> moveText false false
    |> should equal
        "N1b3+"

[<Fact>]
let ``♕axh2 move text`` () =
    { Piece = Queen
      FromCoords = (1, 0)
      ToCoords = (1, 7)
      IsCapture = true
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (7, 7) }    
    |> moveText true true
    |> should equal
        "♕axh2"

[<Fact>]
let ``Position text of initial one`` () =
    Position.init
    |> positionText
    |> should equal
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

[<Fact>]
let ``Position text after first half movement`` () =
    PositionTest.realizedPositionAfterFirstHalfMovement
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

[<Fact>]
let ``Position text after second half movement`` () =
    PositionTest.realizedPositionAfterSecondHalfMovement
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"

[<Fact>]
let ``Position text after third half movement`` () =
    PositionTest.realizedPositionAfterThirdHalfMovement
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"

[<Fact>]
let ``Position text after forth half move`` () =
    PositionTest.realizedPositionAfterForthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 3"

[<Fact>]
let ``Position text after fifth half move`` () =
    PositionTest.realizedPositionAfterFifthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkb1r/ppp1pppp/5n2/3P4/8/8/PPPPBPPP/RNBQK1NR b KQkq - 2 3"

[<Fact>]
let ``Position text after sixth half move`` () =
    PositionTest.realizedPositionAfterSixthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkb1r/ppp2ppp/4pn2/3P4/8/8/PPPPBPPP/RNBQK1NR w KQkq - 0 4"
[<Fact>]
let ``Position text after seventh half move`` () =
    PositionTest.realizedPositionAfterSeventhHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqkb1r/ppp2ppp/4pn2/3P4/7P/8/PPPPBPP1/RNBQK1NR b KQkq h3 0 4"

[<Fact>]
let ``Position text after eighth half move`` () =
    PositionTest.realizedPositionAfterEighthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqk2r/ppp2ppp/4pn2/3P4/7P/b7/PPPPBPP1/RNBQK1NR w KQkq - 1 5"

[<Fact>]
let ``Position text after ninth half move`` () =
    PositionTest.realizedPositionAfterNinthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqk2r/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR b KQkq - 0 5"

[<Fact>]
let ``Position text after tenth half move`` () =
    PositionTest.realizedPositionAfterTentHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQK1NR w KQ - 1 6"

[<Fact>]
let ``Position text after eleventh half move`` () =
    PositionTest.realizedPositionAfterEleventhHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbq1rk1/ppp2ppp/4pn2/3P4/7P/P7/P1PPBPP1/RNBQ1KNR b - - 2 6"

[<Fact>]
let ``Game text`` () =
    { Game.MetaTags = Map.ofArray [| ("White", "me"); ("Black", "you") |]
      Game.InitialPosition = PositionTest.realizedPositionAfterThirdHalfMovement
                             |> PositionTest.unrealizedPosition
      Game.Moves = Seq.ofArray [| PositionTest.forthHalfMove
                                  PositionTest.fifthHalfMove
                                  PositionTest.sixthHalfMove
                                  PositionTest.seventhHalfMove
                                  PositionTest.eighthHalfMove
                                  PositionTest.ninthHalfMove
                                  PositionTest.tenthHalfMove
                                  PositionTest.eleventHalfMove |]
      Game.Result = Some Game.Draw }
    |> gameText
    |> should equal
    <|String.concat "\n"
        [| """[FEN "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"]"""
           """[Black "you"]"""
           """[White "me"]"""
           ""
           "1... Nf6 2. Be2 e6 3. h4 Ba3 4. bxa3 O-O 5. Kf1 1/2-1/2" |]
        