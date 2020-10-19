module BlindfoldChessMechanics.Logic.PositionTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Logic.Position
open BlindfoldChessMechanics.Logic.Board
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
let ``Special white king movements with castling ability`` () =
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
    |> specialKingMovements (0, 4)
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
let ``Special white king movements without castling ability`` () =
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
    |> specialKingMovements (0, 4)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``Special black king movements with castling ability`` () =
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
    |> specialKingMovements (7, 4)
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
let ``Special black king movements without castling ability`` () =
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
    |> specialKingMovements (7, 4)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``White pawn movements with en-passant ability`` () =
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
    |> pawnMovements (4, 1)
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
let ``White pawn movements without en-passant ability`` () =
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
    |> pawnMovements (4, 1)
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
let ``Black pawn movements with promotion ability`` () =
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
    |> pawnMovements (1, 6)
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
let ``White rook piece movements in initial position`` () =
    Position.init
    |> pieceMovements (0, 0)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``White knight piece movements in initial position`` () =
    Position.init
    |> pieceMovements (0, 6)
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
let ``White pawn piece movements in initial position`` () =
    Position.init
    |> pieceMovements (1, 5)
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
let ``Black king piece movements`` () =
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
    |> pieceMovements (7, 4)
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

let realizedPositionAfterFirstHalfMovement =
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
    |> realizedPosition
    |> should equal realizedPositionAfterFirstHalfMovement

let realizedPositionAfterSecondHalfMovement =
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
      EnPassant = Some (5, 3)
      Halfmove = 0
      Fullmove = 2 }
  
[<Fact>]
let ``Position after second half movement`` () =
    realizedPositionAfterFirstHalfMovement
    |> unrealizedPosition
    |> positionAfterMovement { Piece = Pawn
                               FromCoords = (6, 3)
                               ToCoords = (4, 3)
                               IsCapture = false
                               Promotion = None }
    |> realizedPosition
    |> should equal realizedPositionAfterSecondHalfMovement

let realizedPositionAfterThirdHalfMovement =
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
let ``Position after third half movement`` () =
    realizedPositionAfterSecondHalfMovement
    |> unrealizedPosition
    |> positionAfterMovement { Piece = Pawn
                               FromCoords = (3, 4)
                               ToCoords = (4, 3)
                               IsCapture = true
                               Promotion = None }
    |> realizedPosition
    |> should equal realizedPositionAfterThirdHalfMovement

[<Fact>]
let ``Moves in initial position`` () =
    Position.init
    |> movesWithResPos
    |> Seq.map fst
    |> Seq.toArray
    |> should equal [| { Piece = Knight
                         FromCoords = (0, 1)
                         ToCoords = (2, 0)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Knight
                         FromCoords = (0, 1)
                         ToCoords = (2, 2)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Knight
                         FromCoords = (0, 6)
                         ToCoords = (2, 5)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Knight
                         FromCoords = (0, 6)
                         ToCoords = (2, 7)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 0)
                         ToCoords = (2, 0)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 0)
                         ToCoords = (3, 0)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 1)
                         ToCoords = (2, 1)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 1)
                         ToCoords = (3, 1)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 2)
                         ToCoords = (2, 2)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 2)
                         ToCoords = (3, 2)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 3)
                         ToCoords = (2, 3)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 3)
                         ToCoords = (3, 3)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 4)
                         ToCoords = (2, 4)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 4)
                         ToCoords = (3, 4)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 5)
                         ToCoords = (2, 5)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 5)
                         ToCoords = (3, 5)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 6)
                         ToCoords = (2, 6)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 6)
                         ToCoords = (3, 6)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 7)
                         ToCoords = (2, 7)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None }
                       { Piece = Pawn
                         FromCoords = (1, 7)
                         ToCoords = (3, 7)
                         IsCapture = false
                         Promotion = None
                         IsCheck = false
                         IsMate = false
                         IsStalemate = false
                         SamePieceCoords = None } |]

let forthHalfMove = { Piece = Knight
                      FromCoords = (7, 6)
                      ToCoords = (5, 5)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = None }

let realizedPositionAfterForthHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
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
    realizedPositionAfterThirdHalfMovement
    |> unrealizedPosition
    |> positionAfterMove forthHalfMove   
    |> realizedPosition
    |> should equal realizedPositionAfterForthHalfMove

let fifthHalfMove = { Piece = Bishop
                      FromCoords = (0, 5)
                      ToCoords = (1, 4)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = None }

let realizedPositionAfterFifthHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
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
    realizedPositionAfterForthHalfMove
    |> unrealizedPosition
    |> positionAfterMove fifthHalfMove
    |> realizedPosition
    |> should equal realizedPositionAfterFifthHalfMove

let sixthHalfMove = { Piece = Pawn
                      FromCoords = (6, 4)
                      ToCoords = (5, 4)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = None }

let realizedPositionAfterSixthHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
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
    realizedPositionAfterFifthHalfMove
    |> unrealizedPosition
    |> positionAfterMove sixthHalfMove
    |> realizedPosition
    |> should equal realizedPositionAfterSixthHalfMove

let seventhHalfMove = { Piece = Pawn
                        FromCoords = (1, 7)
                        ToCoords = (3, 7)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = None }

let realizedPositionAfterSeventhHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; emptySquare; blackRook|]
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
    realizedPositionAfterSixthHalfMove
    |> unrealizedPosition
    |> positionAfterMove seventhHalfMove
    |> realizedPosition
    |> should equal realizedPositionAfterSeventhHalfMove

let eighthHalfMove = { Piece = Bishop
                       FromCoords = (7, 5)
                       ToCoords = (2, 0)
                       IsCapture = false
                       Promotion = None
                       IsCheck = false
                       IsMate = false
                       IsStalemate = false
                       SamePieceCoords = None }    

let realizedPositionAfterEighthHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   emptySquare; emptySquare; blackRook|]
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
    realizedPositionAfterSeventhHalfMove
    |> unrealizedPosition
    |> positionAfterMove eighthHalfMove  
    |> realizedPosition
    |> should equal realizedPositionAfterEighthHalfMove

let ninthHalfMove = { Piece = Pawn
                      FromCoords = (1, 1)
                      ToCoords = (2, 0)
                      IsCapture = true
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = None }

let realizedPositionAfterNinthHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   emptySquare; emptySquare; blackRook|]
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
    realizedPositionAfterEighthHalfMove
    |> unrealizedPosition
    |> positionAfterMove ninthHalfMove  
    |> realizedPosition
    |> should equal realizedPositionAfterNinthHalfMove

let tenthHalfMove = { Piece = King
                      FromCoords = (7, 4)
                      ToCoords = (7, 6)
                      IsCapture = false
                      Promotion = None
                      IsCheck = false
                      IsMate = false
                      IsStalemate = false
                      SamePieceCoords = None }

let realizedPositionAfterTentHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
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
    realizedPositionAfterNinthHalfMove
    |> unrealizedPosition
    |> positionAfterMove tenthHalfMove
    |> realizedPosition
    |> should equal realizedPositionAfterTentHalfMove

let eleventHalfMove = { Piece = King
                        FromCoords = (0, 4)
                        ToCoords = (0, 5)
                        IsCapture = false
                        Promotion = None
                        IsCheck = false
                        IsMate = false
                        IsStalemate = false
                        SamePieceCoords = None } 

let realizedPositionAfterEleventhHalfMove =
    { Board = [|[|blackRook;   blackKnight; blackBishop; blackQueen;  emptySquare; blackRook;   blackKing;   emptySquare|]
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
    realizedPositionAfterTentHalfMove
    |> unrealizedPosition
    |> positionAfterMove eleventHalfMove
    |> realizedPosition
    |> should equal realizedPositionAfterEleventhHalfMove
