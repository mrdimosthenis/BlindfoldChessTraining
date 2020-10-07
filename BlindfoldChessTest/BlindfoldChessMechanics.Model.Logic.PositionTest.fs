module BlindfoldChessMechanics.Model.Logic.PositionTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Model.Logic.Position
open BlindfoldChessMechanics.Model.Logic.Board

open BlindfoldChessMechanics

[<Fact>]
let ``Special white king moves with castling ability`` () =
    { CurrentBoard = [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMoves (0, 4)

    |> Seq.toArray
    |> should equal
        [| { Piece = King
             FromCoords = (0, 4)
             ToCoords = (0, 6)
             IsCapture = false
             IsCheck = false
             Promotion = None }
           { Piece = King
             FromCoords = (0, 4)
             ToCoords = (0, 2)
             IsCapture = false
             IsCheck = false
             Promotion = None } |]

[<Fact>]
let ``Special white king moves without castling ability`` () =
    { CurrentBoard = [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = false
                   WhiteQueenSideCastle = false
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMoves (0, 4)

    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``Special black king moves with castling ability`` () =
    { CurrentBoard = [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; blackRook|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMoves (7, 4)

    |> Seq.toArray
    |> should equal
        [| { Piece = King
             FromCoords = (7, 4)
             ToCoords = (7, 6)
             IsCapture = false
             IsCheck = false
             Promotion = None }
           { Piece = King
             FromCoords = (7, 4)
             ToCoords = (7, 2)
             IsCapture = false
             IsCheck = false
             Promotion = None } |]

[<Fact>]
let ``Special black king moves without castling ability`` () =
    { CurrentBoard = [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   blackBishop; emptySquare; blackRook|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; whiteRook;   emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMoves (7, 4)

    |> Seq.toArray
    |> should equal
        [||]