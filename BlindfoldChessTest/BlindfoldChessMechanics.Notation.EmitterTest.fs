module BlindfoldChessMechanics.Notation.EmitterTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Notation.Emitter
open BlindfoldChessMechanics.Logic.Board
open BlindfoldChessMechanics.Logic.Position
open BlindfoldChessMechanics.Logic

[<Fact>]
let ``0-0 move text`` () =
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
        "0-0"

[<Fact>]
let ``0-0-0 move text`` () =
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
        "0-0-0"

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
        "rnbqk2r/ppp1pppp/5n2/3P4/8/b7/PPPPBPPP/RNBQK1NR w KQkq - 3 4"

[<Fact>]
let ``Position text after sevent half move`` () =
    PositionTest.realizedPositionAfterSeventhHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbqk2r/ppp1pppp/5n2/3P4/8/P7/P1PPBPPP/RNBQK1NR b KQkq - 0 4"

[<Fact>]
let ``Position text after nineth half move`` () =
    PositionTest.realizedPositionAfterNinenthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbq1rk1/ppp1pppp/5n2/3P4/8/P7/P1PPBPPP/RNBQK1NR w KQ - 1 5"

[<Fact>]
let ``Position text after tenth half move`` () =
    PositionTest.realizedPositionAfterTenthHalfMove
    |> PositionTest.unrealizedPosition
    |> positionText
    |> should equal
        "rnbq1rk1/ppp1pppp/5n2/3P4/8/P7/P1PPBPPP/RNBQ1KNR b - - 2 5"