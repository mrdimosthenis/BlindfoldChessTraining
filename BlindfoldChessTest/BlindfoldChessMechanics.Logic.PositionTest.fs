﻿module BlindfoldChessMechanics.Logic.PositionTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Logic.Position
open BlindfoldChessMechanics.Logic.Board
open FSharpx.Collections

// tests

[<Fact>]
let ``Special white king movements with castling ability`` () =
    { Board = Array.rev
                  [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                    [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMovements (0, 4)
    |> LazyList.toList
    |> should equal
        [ { Piece = King
            FromCoords = (0, 4)
            ToCoords = (0, 6)
            IsCapture = false
            Promotion = None }
          { Piece = King
            FromCoords = (0, 4)
            ToCoords = (0, 2)
            IsCapture = false
            Promotion = None } ]

[<Fact>]
let ``Special white king movements without castling ability`` () =
    { Board = Array.rev
                [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = false
                   WhiteQueenSideCastle = false
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMovements (0, 4)
    |> LazyList.toList
    |> should be Empty

[<Fact>]
let ``Special black king movements with castling ability`` () =
    { Board = Array.rev
                [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; blackRook|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMovements (7, 4)
    |> LazyList.toList
    |> should equal
        [ { Piece = King
            FromCoords = (7, 4)
            ToCoords = (7, 6)
            IsCapture = false
            Promotion = None }
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (7, 2)
            IsCapture = false
            Promotion = None } ]

[<Fact>]
let ``Special black king movements without castling ability`` () =
    { Board = Array.rev
                [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   blackBishop; emptySquare; blackRook|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; whiteRook;   emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> specialKingMovements (7, 4)
    |> LazyList.toList
    |> should be Empty

[<Fact>]
let ``White pawn movements with en-passant ability`` () =
    { Board = Array.rev
                [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = Some (5, 2)
      Halfmove = 12
      Fullmove = 20 }
    |> pawnMovements (4, 1)
    |> LazyList.toList
    |> should equal
        [ { Piece = Pawn
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
            Promotion = None } ]

[<Fact>]
let ``White pawn movements without en-passant ability`` () =
    { Board = Array.rev
                [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> pawnMovements (4, 1)
    |> LazyList.toList
    |> should equal
        [ { Piece = Pawn
            FromCoords = (4, 1)
            ToCoords = (5, 1)
            IsCapture = false
            Promotion = None }
          { Piece = Pawn
            FromCoords = (4, 1)
            ToCoords = (5, 0)
            IsCapture = true
            Promotion = None } ]

[<Fact>]
let ``Black pawn movements with promotion ability`` () =
    { Board = Array.rev
                [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; whitePawn;   blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = Some (3, 1)
      Halfmove = 12
      Fullmove = 20 }
    |> pawnMovements (1, 6)
    |> LazyList.toList
    |> should equal
        [ { Piece = Pawn
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
            Promotion = Some Knight } ]

[<Fact>]
let ``White rook piece movements in initial position`` () =
    Position.init
    |> pieceMovements (0, 0)
    |> LazyList.toList
    |> should be Empty

[<Fact>]
let ``White knight piece movements in initial position`` () =
    Position.init
    |> pieceMovements (0, 6)
    |> LazyList.toList
    |> should equal
        [ { Piece = Knight
            FromCoords = (0, 6)
            ToCoords = (2, 5)
            IsCapture = false
            Promotion = None }
          { Piece = Knight
            FromCoords = (0, 6)
            ToCoords = (2, 7)
            IsCapture = false
            Promotion = None } ]

[<Fact>]
let ``White pawn piece movements in initial position`` () =
    Position.init
    |> pieceMovements (1, 5)
    |> LazyList.toList
    |> should equal
        [ { Piece = Pawn
            FromCoords = (1, 5)
            ToCoords = (2, 5)
            IsCapture = false
            Promotion = None }
          { Piece = Pawn
            FromCoords = (1, 5)
            ToCoords = (3, 5)
            IsCapture = false
            Promotion = None } ]

[<Fact>]
let ``Black king piece movements`` () =
    { Board = Array.rev
                [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; blackRook|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; whiteRook;   emptySquare;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true }
      EnPassant = None
      Halfmove = 12
      Fullmove = 20 }
    |> pieceMovements (7, 4)
    |> LazyList.toList
    |> should equal
        [ { Piece = King
            FromCoords = (7, 4)
            ToCoords = (7, 6)
            IsCapture = false
            Promotion = None }
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (6, 3)
            IsCapture = false
            Promotion = None }
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (6, 4)
            IsCapture = false
            Promotion = None };
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (6, 5)
            IsCapture = true
            Promotion = None }
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (7, 3)
            IsCapture = false
            Promotion = None }
          { Piece = King
            FromCoords = (7, 4)
            ToCoords = (7, 5)
            IsCapture = false
            Promotion = None } ] 

let positionAfterFirstHalfMovement =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   emptySquare; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = Some (2, 4)
      Halfmove = 0
      Fullmove = 1 }

[<Fact>]
let ``Position after first half movement`` () =
    Position.init
    |> positionAfterMovement { Piece = Pawn
                               FromCoords = (1, 4)
                               ToCoords = (3, 4)
                               IsCapture = false
                               Promotion = None }
    |> should equal positionAfterFirstHalfMovement

let positionAfterSecondHalfMovement =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; blackPawn;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   emptySquare; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = Some (5, 3)
      Halfmove = 0
      Fullmove = 2 }
  
[<Fact>]
let ``Position after second half movement`` () =
    positionAfterFirstHalfMovement
    |> positionAfterMovement { Piece = Pawn
                               FromCoords = (6, 3)
                               ToCoords = (4, 3)
                               IsCapture = false
                               Promotion = None }
    |> should equal positionAfterSecondHalfMovement

let positionAfterThirdHalfMovement =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; blackPawn;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   emptySquare; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 0
      Fullmove = 2 }

[<Fact>]
let ``Position after third half movement`` () =
    positionAfterSecondHalfMovement
    |> positionAfterMovement { Piece = Pawn
                               FromCoords = (3, 4)
                               ToCoords = (4, 3)
                               IsCapture = true
                               Promotion = None }
    |> should equal positionAfterThirdHalfMovement

[<Fact>]
let ``Moves in initial position`` () =
    Position.init
    |> movesWithResultedPosition
    |> LazyList.map fst
    |> LazyList.toList
    |> should equal [ { Piece = Knight
                        FromCoords = (0, 1)
                        ToCoords = (2, 0)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Knight
                        FromCoords = (0, 1)
                        ToCoords = (2, 2)
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
                        FromCoords = (0, 6)
                        ToCoords = (2, 7)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 0)
                        ToCoords = (2, 0)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 0)
                        ToCoords = (3, 0)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 1)
                        ToCoords = (2, 1)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 1)
                        ToCoords = (3, 1)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 2)
                        ToCoords = (2, 2)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 2)
                        ToCoords = (3, 2)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 3)
                        ToCoords = (2, 3)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 3)
                        ToCoords = (3, 3)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 4)
                        ToCoords = (2, 4)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 4)
                        ToCoords = (3, 4)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 5)
                        ToCoords = (2, 5)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 5)
                        ToCoords = (3, 5)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 6)
                        ToCoords = (2, 6)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 6)
                        ToCoords = (3, 6)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 7)
                        ToCoords = (2, 7)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }
                      { Piece = Pawn
                        FromCoords = (1, 7)
                        ToCoords = (3, 7)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] } ]

let forthHalfMove = { Piece = Knight
                      FromCoords = (7, 6)
                      ToCoords = (5, 5)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = [||] }

let positionAfterForthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; blackPawn;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   emptySquare; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 1
      Fullmove = 3 }

[<Fact>]
let ``Position after forth half move`` () =
    positionAfterThirdHalfMovement
    |> positionAfterMove forthHalfMove
    |> should equal positionAfterForthHalfMove

let fifthHalfMove = { Piece = Bishop
                      FromCoords = (0, 5)
                      ToCoords = (1, 4)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = [||] }

let positionAfterFifthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; blackPawn;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 2
      Fullmove = 3 }

[<Fact>]
let ``Position after fifth half move`` () =
    positionAfterForthHalfMove
    |> positionAfterMove fifthHalfMove
    |> should equal positionAfterFifthHalfMove

let sixthHalfMove = { Piece = Pawn
                      FromCoords = (6, 4)
                      ToCoords = (5, 4)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = [||] }

let positionAfterSixthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   whitePawn|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 0
      Fullmove = 4 }

[<Fact>]
let ``Position after sixth half move`` () =
    positionAfterFifthHalfMove
    |> positionAfterMove sixthHalfMove
    |> should equal positionAfterSixthHalfMove

let seventhHalfMove = { Piece = Pawn
                        FromCoords = (1, 7)
                        ToCoords = (3, 7)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] }

let positionAfterSeventhHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = Some (2, 7)
      Halfmove = 0
      Fullmove = 4 }

[<Fact>]
let ``Position after seventh half move`` () =
    positionAfterSixthHalfMove
    |> positionAfterMove seventhHalfMove
    |> should equal positionAfterSeventhHalfMove

let eighthHalfMove = { Piece = Bishop
                       FromCoords = (7, 5)
                       ToCoords = (2, 0)
                       IsCapture = false
                       Promotion = None
                       IsCheck = false
                       IsMate = false
                       IsStalemate = false
                       SamePieceCoords = [||] }    

let positionAfterEighthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   emptySquare; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 1
      Fullmove = 5 }

[<Fact>]
let ``Position after eighth half move`` () =
    positionAfterSeventhHalfMove
    |> positionAfterMove eighthHalfMove
    |> should equal positionAfterEighthHalfMove

let ninthHalfMove = { Piece = Pawn
                      FromCoords = (1, 1)
                      ToCoords = (2, 0)
                      IsCapture = true
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = [||] }

let positionAfterNinthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   emptySquare; emptySquare; blackRook|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   emptySquare; whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = true
                   BlackQueenSideCastle = true } 
      EnPassant = None
      Halfmove = 0
      Fullmove = 5 }

[<Fact>]
let ``Position after ninth half move`` () =
    positionAfterEighthHalfMove
    |> positionAfterMove ninthHalfMove
    |> should equal positionAfterNinthHalfMove

let tenthHalfMove = { Piece = King
                      FromCoords = (7, 4)
                      ToCoords = (7, 6)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = [||] }

let positionAfterTentHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   emptySquare; whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   emptySquare; whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = true
                   WhiteQueenSideCastle = true
                   BlackKingSideCastle = false
                   BlackQueenSideCastle = false } 
      EnPassant = None
      Halfmove = 1
      Fullmove = 6 }

[<Fact>]
let ``Position after tenth half move`` () =
    positionAfterNinthHalfMove
    |> positionAfterMove tenthHalfMove
    |> should equal positionAfterTentHalfMove

let eleventHalfMove = { Piece = King
                        FromCoords = (0, 4)
                        ToCoords = (0, 5)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] } 

let positionAfterEleventhHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
                  [|blackPawn;   blackPawn;   blackPawn;   emptySquare; emptySquare;   blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   emptySquare; whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  emptySquare; whiteKing;   whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = false
                   WhiteQueenSideCastle = false
                   BlackKingSideCastle = false
                   BlackQueenSideCastle = false } 
      EnPassant = None
      Halfmove = 2
      Fullmove = 6 }

[<Fact>]
let ``Position after eleventh half move`` () =
    positionAfterTentHalfMove
    |> positionAfterMove eleventHalfMove
    |> should equal positionAfterEleventhHalfMove

let twelfthHalfMove = { Piece = Pawn
                        FromCoords = (6, 2)
                        ToCoords = (4, 2)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = [||] } 

let positionAfterTwelfthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
                  [|blackPawn;   blackPawn;   emptySquare; emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; blackPawn;   whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   emptySquare; whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  emptySquare; whiteKing;   whiteKnight; whiteRook|]|]
      IsWhiteToMove = true
      Castling = { WhiteKingSideCastle = false
                   WhiteQueenSideCastle = false
                   BlackKingSideCastle = false
                   BlackQueenSideCastle = false } 
      EnPassant = Some (5, 2)
      Halfmove = 0
      Fullmove = 7 }

[<Fact>]
let ``Position after twelfth half move`` () =
    positionAfterEleventhHalfMove
    |> positionAfterMove twelfthHalfMove
    |> should equal positionAfterTwelfthHalfMove

let thirteenthHalfMove = { Piece = Pawn
                           FromCoords = (4, 3)
                           ToCoords = (5, 2)
                           IsCapture = true
                           Promotion = None
                           IsCheck = false
                           IsMate = false
                           IsStalemate = false
                           SamePieceCoords = [||] } 

let positionAfterThirteenthHalfMove =
    { Board = Array.rev
                [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
                  [|blackPawn;   blackPawn;   emptySquare; emptySquare; emptySquare; blackPawn;   blackPawn;   blackPawn|]
                  [|emptySquare; emptySquare; whitePawn;   emptySquare; blackPawn;   blackKnight; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
                  [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
                  [|whitePawn;   emptySquare; whitePawn;   whitePawn;   whiteBishop; whitePawn;   whitePawn;   emptySquare|]
                  [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  emptySquare; whiteKing;   whiteKnight; whiteRook|]|]
      IsWhiteToMove = false
      Castling = { WhiteKingSideCastle = false
                   WhiteQueenSideCastle = false
                   BlackKingSideCastle = false
                   BlackQueenSideCastle = false } 
      EnPassant = None
      Halfmove = 0
      Fullmove = 7 }

[<Fact>]
let ``Position after thirteenth half move`` () =
    positionAfterTwelfthHalfMove
    |> positionAfterMove thirteenthHalfMove
    |> should equal positionAfterThirteenthHalfMove
