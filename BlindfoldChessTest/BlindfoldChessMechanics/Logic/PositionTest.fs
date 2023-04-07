module BlindfoldChessTest.BlindfoldChessMechanics.Logic.PositionTest

open BlindfoldChessMechanics.Logic.Board
open BlindfoldChessMechanics.Logic.Position
open FSharpx.Collections
open NUnit.Framework

[<Test>]
let ``Special white king movements with castling ability`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
      |> specialKingMovements (0, 4)
      |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
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
      HalfMove = 12
      FullMove = 20 }
    |> specialKingMovements (0, 4)
    |> LazyList.toList
    |> Assert.IsEmpty

[<Test>]
let ``Special black king movements with castling ability`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
        |> specialKingMovements (7, 4)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
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
      HalfMove = 12
      FullMove = 20 }
    |> specialKingMovements (7, 4)
    |> LazyList.toList
    |> Assert.IsEmpty

[<Test>]
let ``White pawn movements with en-passant ability`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
        |> pawnMovements (4, 1)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
let ``White pawn movements without en-passant ability`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
        |> pawnMovements (4, 1)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
let ``Black pawn movements with promotion ability`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
        |> pawnMovements (1, 6)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
let ``White rook piece movements in initial position`` () =
    init
    |> pieceMovements (0, 0)
    |> LazyList.toList
    |> Assert.IsEmpty

[<Test>]
let ``White knight piece movements in initial position`` () =
    let act =
        init
        |> pieceMovements (0, 6)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
let ``White pawn piece movements in initial position`` () =
    let act =
        init
        |> pieceMovements (1, 5)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

[<Test>]
let ``Black king piece movements`` () =
    let act =
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
          HalfMove = 12
          FullMove = 20 }
        |> pieceMovements (7, 4)
        |> LazyList.toList
    let exp =
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
    Assert.AreEqual(exp, act)

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
      HalfMove = 0
      FullMove = 1 }

[<Test>]
let ``Position after first half movement`` () =
    let act =
        init
        |> positionAfterMovement { Piece = Pawn
                                   FromCoords = (1, 4)
                                   ToCoords = (3, 4)
                                   IsCapture = false
                                   Promotion = None }
    Assert.AreEqual(positionAfterFirstHalfMovement, act)

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
      HalfMove = 0
      FullMove = 2 }
  
[<Test>]
let ``Position after second half movement`` () =
    let act =
        positionAfterFirstHalfMovement
        |> positionAfterMovement { Piece = Pawn
                                   FromCoords = (6, 3)
                                   ToCoords = (4, 3)
                                   IsCapture = false
                                   Promotion = None }
    Assert.AreEqual(positionAfterSecondHalfMovement, act)

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
      HalfMove = 0
      FullMove = 2 }

[<Test>]
let ``Position after third half movement`` () =
    let act =
        positionAfterSecondHalfMovement
        |> positionAfterMovement { Piece = Pawn
                                   FromCoords = (3, 4)
                                   ToCoords = (4, 3)
                                   IsCapture = true
                                   Promotion = None }
    Assert.AreEqual(positionAfterThirdHalfMovement, act)

[<Test>]
let ``Moves in initial position`` () =
    let act =
        init
        |> movesWithResultedPosition
        |> LazyList.map fst
        |> LazyList.toList
    let exp =
           [ { Piece = Knight
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
    Assert.AreEqual(exp, act)

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
      HalfMove = 1
      FullMove = 3 }

[<Test>]
let ``Position after forth half move`` () =
    let act =
        positionAfterThirdHalfMovement
        |> positionAfterMove forthHalfMove
    Assert.AreEqual(positionAfterForthHalfMove, act)

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
      HalfMove = 2
      FullMove = 3 }

[<Test>]
let ``Position after fifth half move`` () =
    let act =
        positionAfterForthHalfMove
        |> positionAfterMove fifthHalfMove
    Assert.AreEqual(positionAfterFifthHalfMove, act)

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
      HalfMove = 0
      FullMove = 4 }

[<Test>]
let ``Position after sixth half move`` () =
    let act =
        positionAfterFifthHalfMove
        |> positionAfterMove sixthHalfMove
    Assert.AreEqual(positionAfterSixthHalfMove, act)

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
      HalfMove = 0
      FullMove = 4 }

[<Test>]
let ``Position after seventh half move`` () =
    let act =
        positionAfterSixthHalfMove
        |> positionAfterMove seventhHalfMove
    Assert.AreEqual(positionAfterSeventhHalfMove, act)

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
      HalfMove = 1
      FullMove = 5 }

[<Test>]
let ``Position after eighth half move`` () =
    let act =
        positionAfterSeventhHalfMove
        |> positionAfterMove eighthHalfMove
    Assert.AreEqual(positionAfterEighthHalfMove, act)

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
      HalfMove = 0
      FullMove = 5 }

[<Test>]
let ``Position after ninth half move`` () =
    let act =
        positionAfterEighthHalfMove
        |> positionAfterMove ninthHalfMove
    Assert.AreEqual(positionAfterNinthHalfMove, act)

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
      HalfMove = 1
      FullMove = 6 }

[<Test>]
let ``Position after tenth half move`` () =
    let act =
        positionAfterNinthHalfMove
        |> positionAfterMove tenthHalfMove
    Assert.AreEqual(positionAfterTentHalfMove, act)

let eleventhHalfMove = { Piece = King
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
      HalfMove = 2
      FullMove = 6 }

[<Test>]
let ``Position after eleventh half move`` () =
    let act =
        positionAfterTentHalfMove
        |> positionAfterMove eleventhHalfMove
    Assert.AreEqual(positionAfterEleventhHalfMove, act)

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
      HalfMove = 0
      FullMove = 7 }

[<Test>]
let ``Position after twelfth half move`` () =
    let act =
        positionAfterEleventhHalfMove
        |> positionAfterMove twelfthHalfMove
    Assert.AreEqual(positionAfterTwelfthHalfMove, act)

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
      HalfMove = 0
      FullMove = 7 }

[<Test>]
let ``Position after thirteenth half move`` () =
    let act =
        positionAfterTwelfthHalfMove
        |> positionAfterMove thirteenthHalfMove
    Assert.AreEqual(positionAfterThirteenthHalfMove, act)
    