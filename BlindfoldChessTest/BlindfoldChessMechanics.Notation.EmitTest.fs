module BlindfoldChessMechanics.Notation.EmitTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Notation.Emit
open BlindfoldChessMechanics.Logic.Board
open BlindfoldChessMechanics.Logic.Position

[<Fact>]
let ``0-0 movement name`` () =
    { Piece = King
      FromCoords = (0, 4)
      ToCoords = (0, 6)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }    
    |> movementName true false
    |> should equal
        "0-0"

[<Fact>]
let ``0-0-0 movement name`` () =
    { Piece = King
      FromCoords = (7, 4)
      ToCoords = (7, 2)
      IsCapture = false
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = None }    
    |> movementName false true
    |> should equal
        "0-0-0"

[<Fact>]
let ``bxc3 movement name`` () =
    { Piece = Pawn
      FromCoords = (1, 3)
      ToCoords = (2, 2)
      IsCapture = true
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (1, 1) }    
    |> movementName true false
    |> should equal
        "dxc3"

[<Fact>]
let ``gxh1=♜# movement name`` () =
    { Piece = Pawn
      FromCoords = (1, 6)
      ToCoords = (0, 7)
      IsCapture = true
      Promotion = Some Rook
      IsCheck = true
      IsMate = true
      IsStalemate = false
      SamePieceCoords = None }    
    |> movementName false true
    |> should equal
        "gxh1=♜#"

[<Fact>]
let ``N1b3+ movement name`` () =
    { Piece = Knight
      FromCoords = (0, 0)
      ToCoords = (2, 1)
      IsCapture = false
      Promotion = None
      IsCheck = true
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (4, 0) }    
    |> movementName false false
    |> should equal
        "N1b3+"

[<Fact>]
let ``♕axh2 movement name`` () =
    { Piece = Queen
      FromCoords = (1, 0)
      ToCoords = (1, 7)
      IsCapture = true
      Promotion = None
      IsCheck = false
      IsMate = false
      IsStalemate = false
      SamePieceCoords = Some (7, 7) }    
    |> movementName true true
    |> should equal
        "♕axh2"

