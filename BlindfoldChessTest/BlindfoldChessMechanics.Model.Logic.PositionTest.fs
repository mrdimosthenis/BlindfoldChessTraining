module BlindfoldChessMechanics.Model.Logic.PositionTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Model.Logic.Position
open BlindfoldChessMechanics.Model.Logic.Board
open BlindfoldChessMechanics

// types

type RealizedPosition = { Board: Board.Resident array array
                          IsWhiteToMove: bool
                          Castling: Castling 
                          EnPassant: Board.Coordinates Option
                          Halfmove: int
                          Fullmove: int }

// functions

let realizedPosition (position: Position): RealizedPosition =
    { Board = position.Board
                     |> Seq.rev
                     |> Utils.seqToArrays
      IsWhiteToMove = position.IsWhiteToMove
      Castling = position.Castling 
      EnPassant = position.EnPassant
      Halfmove = position.Halfmove
      Fullmove = position.Fullmove }

let unrealizedPosition (realizedPosition: RealizedPosition): Position =
    { Board = realizedPosition.Board
                     |> Utils.seqOfArrays
                     |> Seq.rev
      IsWhiteToMove = realizedPosition.IsWhiteToMove
      Castling = realizedPosition.Castling 
      EnPassant = realizedPosition.EnPassant
      Halfmove = realizedPosition.Halfmove
      Fullmove = realizedPosition.Fullmove }

// tests

[<Fact>]
let ``Special white king moves with castling ability`` () =
    { Board = [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
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
    |> unrealizedPosition
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
    { Board = [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
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
    |> unrealizedPosition
    |> specialKingMoves (0, 4)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``Special black king moves with castling ability`` () =
    { Board = [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; blackRook|]
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
    |> unrealizedPosition
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
    { Board = [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   blackBishop; emptySquare; blackRook|]
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
    |> unrealizedPosition
    |> specialKingMoves (7, 4)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``White pawn  moves with en-passant ability`` () =
    { Board = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
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
      EnPassant = Some (4, 2)
      Halfmove = 12
      Fullmove = 20 }
    |> unrealizedPosition
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
    { Board = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
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
    |> unrealizedPosition
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
    { Board = [|[|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
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
    |> unrealizedPosition
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

[<Fact>]
let ``White rook piece move in initial position`` () =
    Position.init
    |> pieceMoves (0, 0)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``White knight piece move in initial position`` () =
    Position.init
    |> pieceMoves (0, 6)
    |> Seq.toArray
    |> should equal
        [| { Piece = Knight
             FromCoords = (0, 6)
             ToCoords = (2, 5)
             IsCapture = false
             Promotion = None }
           { Piece = Knight
             FromCoords = (0, 6)
             ToCoords = (2, 7)
             IsCapture = false
             Promotion = None } |]

[<Fact>]
let ``White pawn piece move in initial position`` () =
    Position.init
    |> pieceMoves (1, 5)
    |> Seq.toArray
    |> should equal
        [| { Piece = Pawn
             FromCoords = (1, 5)
             ToCoords = (2, 5)
             IsCapture = false
             Promotion = None }
           { Piece = Pawn
             FromCoords = (1, 5)
             ToCoords = (3, 5)
             IsCapture = false
             Promotion = None } |]

[<Fact>]
let ``Black king piece moves`` () =
    { Board = [|[|blackRook;   emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; blackRook|]
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
    |> unrealizedPosition
    |> pieceMoves (7, 4)
    |> Seq.toArray
    |> should equal
        [| { Piece = King
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
             Promotion = None } |] 

let realizedPositionAfterFirstHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
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
      EnPassant = Some (3, 4)
      Halfmove = 0
      Fullmove = 1 }

[<Fact>]
let ``Position after first half move`` () =
    Position.init
    |> positionAfterMove { Piece = Pawn
                           FromCoords = (1, 4)
                           ToCoords = (3, 4)
                           IsCapture = false
                           Promotion = None }
    |> realizedPosition
    |> should equal realizedPositionAfterFirstHalfMove

let realizedPositionAfterSecondHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
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
      EnPassant = Some (4, 3)
      Halfmove = 0
      Fullmove = 2 }
  
[<Fact>]
let ``Position after second half move`` () =
    realizedPositionAfterFirstHalfMove
    |> unrealizedPosition
    |> positionAfterMove { Piece = Pawn
                           FromCoords = (6, 3)
                           ToCoords = (4, 3)
                           IsCapture = false
                           Promotion = None }
    |> realizedPosition
    |> should equal realizedPositionAfterSecondHalfMove

let realizedPositionAfterThirdHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
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
let ``Position after third half move`` () =
    realizedPositionAfterSecondHalfMove
    |> unrealizedPosition
    |> positionAfterMove { Piece = Pawn
                           FromCoords = (3, 4)
                           ToCoords = (4, 3)
                           IsCapture = true
                           Promotion = None }
    |> realizedPosition
    |> should equal realizedPositionAfterThirdHalfMove
