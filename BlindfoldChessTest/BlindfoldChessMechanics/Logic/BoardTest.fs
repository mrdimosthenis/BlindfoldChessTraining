module BlindfoldChessTest.BlindfoldChessMechanics.Logic.BoardTest

open BlindfoldChessMechanics.Logic.Board
open FSharpx.Collections
open NUnit.Framework

[<Test>]
let ``Coordinates controlled by rook`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare; whiteBishop; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByRook (4, 2)
        |> LazyList.toList
    let exp =
           [ (7, 2); (6, 2); (5, 2)
             (1, 2); (2, 2); (3, 2)
             (4, 5); (4, 4); (4, 3)
             (4, 0); (4, 1) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by ghost rook`` () =
    Assert.Throws<NoPiece>(fun () ->
        coordinatesControlledByRook (4, 2) empty |> LazyList.toList |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by bishop`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByBishop (4, 2)
        |> LazyList.toList
    let exp =
           [ (7, 5); (6, 4); (5, 3)
             (0, 6); (1, 5); (2, 4); (3, 3)
             (5, 1)
             (3, 1) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by ghost bishop`` () =
    Assert.Throws<NoPiece>(fun () ->
        coordinatesControlledByBishop (4, 2) empty |> LazyList.toList |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by queen`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|blackKnight; emptySquare; emptySquare; emptySquare; blackQueen;  emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
          [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByQueen (4, 4)
        |> LazyList.toList
    let exp =
           [ (7, 4); (6, 4); (5, 4)
             (2, 4); (3, 4)
             (4, 7); (4, 6); (4, 5)
             (4, 1); (4, 2); (4, 3)
             (7, 7); (6, 6); (5, 5)
             (1, 7); (2, 6); (3, 5)
             (7, 1); (6, 2); (5, 3)
             (0, 0); (1, 1); (2, 2); (3, 3) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by ghost queen`` () =
    Assert.Throws<NoPiece>(fun () ->
        coordinatesControlledByQueen (4, 2) empty |> LazyList.toList |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by knight in the middle`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByKnight (2, 5)
        |> LazyList.toList
    let exp =
           [ (0, 4); (1, 3); (3, 3); (0, 6); (1, 7); (4, 6); (3, 7) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by knight in the corner`` () =
    let act =
        [|[|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whiteKnight; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByKnight (6, 0)
        |> LazyList.toList
    let exp =
           [ (5, 2); (7, 2) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by ghost knight`` () =
    Assert.Throws<WrongPiece>(fun () ->
        coordinatesControlledByKnight (4, 2) empty |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by king in the middle`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; whiteRook;   whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByKing (2, 3)
        |> LazyList.toList
    let exp =
           [ (1, 2); (1, 3); (1, 4); (2, 4); (3, 2); (3, 3); (3, 4) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by king in the corner`` () =
    let act =
        [|[|blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whiteQueen;  blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByKing (7, 0)
        |> LazyList.toList
    let exp =
           [ (6, 0); (7, 1) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by ghost king`` () =
    Assert.Throws<WrongPiece>(fun () ->
        coordinatesControlledByKing (4, 2) empty |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by white init pawn in the edge`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByPawn (1, 7)
        |> LazyList.toList
    let exp =
           [ (2, 7); (3, 7); (2, 6) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by white init blocked pawn`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByPawn (1, 3)
        |> LazyList.toList
    let exp =
           [ (2, 3) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by black blocked pawn`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; whiteRook;   whiteRook;   whiteRook;   emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByPawn (4, 3)
        |> LazyList.toList
    let exp =
           [ (3, 2); (3, 4) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by black init pawn in the edge`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Array.rev
    |> coordinatesControlledByPawn (6, 0)
    |> LazyList.toList
    |> Assert.IsEmpty

[<Test>]
let ``Coordinates controlled by ghost pawn`` () =
    Assert.Throws<WrongPiece>(fun () ->
        coordinatesControlledByKing (4, 2) empty |> ignore
    )
    |> ignore

[<Test>]
let ``Coordinates controlled by white color`` () =
    let act =
        [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whiteKing;   blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; whiteBishop; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; blackPawn;   emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
          [|blackPawn;   emptySquare; blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByColor true
        |> LazyList.toList
    let exp =
           [ (3, 4)
             (7, 7); (6, 6); (5, 5); (4, 4); (5, 1); (4, 2); (0, 0); (1, 1); (2, 2)
             (6, 1); (7, 0); (7, 1) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Coordinates controlled by black color`` () =
    let act =
        [|[|blackRook;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare|]
          [|emptySquare; blackPawn;   emptySquare; emptySquare; blackQueen;  blackPawn;   blackPawn;   emptySquare|]
          [|blackPawn;   emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare; blackBishop; blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
          [|whitePawn;   whitePawn;   blackPawn;   whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; whiteQueen;  emptySquare; whitePawn;   whitePawn;   whitePawn|]
          [|emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare|]|]
        |> Array.rev
        |> coordinatesControlledByColor false
        |> LazyList.toList
    let exp =
           [ (2, 2)
             (4, 0)
             (6, 7); (4, 7); (0, 1); (1, 2); (2, 3); (3, 4); (4, 5)
             (5, 1); (4, 1)
             (7, 4); (6, 2); (6, 3); (7, 5); (3, 7); (4, 6); (5, 5); (7, 3); (3, 1); (4, 2); (5, 3)
             (6, 0); (7, 2); (7, 1)
             (7, 7) ]
    Assert.AreEqual(exp, act)

[<Test>]
let ``Is white king in danger in initial board`` () =
    init
    |> isKingInDanger true
    |> Assert.False

[<Test>]
let ``Is black king in danger in initial board`` () =
    init
    |> isKingInDanger false
    |> Assert.False

[<Test>]
let ``Is white king in danger`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whiteKing;   emptySquare; whitePawn;   emptySquare; blackRook;   emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Array.rev
    |> isKingInDanger true
    |> Assert.False

[<Test>]
let ``Is black king in danger`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Array.rev
    |> isKingInDanger false
    |> Assert.True
        