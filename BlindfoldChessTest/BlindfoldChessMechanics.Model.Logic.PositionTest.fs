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
             Promotion = None }
           { Piece = King
             FromCoords = (0, 4)
             ToCoords = (0, 2)
             IsCapture = false
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
             Promotion = None }
           { Piece = King
             FromCoords = (7, 4)
             ToCoords = (7, 2)
             IsCapture = false
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

[<Fact>]
let ``White pawn  moves with en-passant ability`` () =
    { CurrentBoard = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = Some (4, 2)
      Halfmove = 12
      Fullmove = 20 }
    |> pawnMoves (4, 1)
    |> Seq.toArray
    |> should equal
        [| { Piece = Pawn
             FromCoords = (4, 1)
             ToCoords = (5, 2)
             IsCapture = true
             Promotion = None }
           { Piece = Pawn
             FromCoords = (4, 1)
             ToCoords = (5, 1)
             IsCapture = false
             Promotion = None }
           { Piece = Pawn
             FromCoords = (4, 1)
             ToCoords = (5, 0)
             IsCapture = true
             Promotion = None } |]

[<Fact>]
let ``White pawn moves without en-passant ability`` () =
    { CurrentBoard = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]|]
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
    |> pawnMoves (4, 1)
    |> Seq.toArray
    |> should equal
        [| { Piece = Pawn
             FromCoords = (4, 1)
             ToCoords = (5, 1)
             IsCapture = false
             Promotion = None }
           { Piece = Pawn
             FromCoords = (4, 1)
             ToCoords = (5, 0)
             IsCapture = true
             Promotion = None } |]

[<Fact>]
let ``Black pawn moves with promotion ability`` () =
    { CurrentBoard = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare|]
                       [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = Some (3, 1)
      Halfmove = 12
      Fullmove = 20 }
    |> pawnMoves (1, 6)
    |> Seq.toArray
    |> should equal
        [| { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 6)
             IsCapture = false
             Promotion = Some Queen }
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 6)
             IsCapture = false
             Promotion = Some Rook }
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 6)
             IsCapture = false
             Promotion = Some Bishop }
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 6)
             IsCapture = false
             Promotion = Some Knight };
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 7)
             IsCapture = true
             Promotion = Some Queen }
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 7)
             IsCapture = true
             Promotion = Some Rook };
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 7)
             IsCapture = true
             Promotion = Some Bishop }
           { Piece = Pawn
             FromCoords = (1, 6)
             ToCoords = (0, 7)
             IsCapture = true
             Promotion = Some Knight } |]
