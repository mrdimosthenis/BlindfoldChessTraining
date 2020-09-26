module BlindfoldChessMechanics.Model.BoardTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Model.Board

[<Fact>]
let ``Board after valid pawn move`` () =
    Board.init
    |> afterMove (1, 0, 3, 0)
    |> Option.get
    |> Seq.rev
    |> toArrays
    |> should equal
        [|[|blackRook; blackKnight; blackBishop; blackQueen; blackKing; blackBishop; blackKnight; blackRook|]
          [|blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whitePawn; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn|]
          [|whiteRook; whiteKnight; whiteBishop; whiteQueen; whiteKing; whiteBishop; whiteKnight; whiteRook|]|]

[<Fact>]
let ``Board after valid knight move`` () =
    Board.init
    |> afterMove (0, 6, 2, 5)
    |> Option.get
    |> Seq.rev
    |> toArrays
    |> should equal
        [|[|blackRook; blackKnight; blackBishop; blackQueen; blackKing; blackBishop; blackKnight; blackRook|]
          [|blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
          [|whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn|]
          [|whiteRook; whiteKnight; whiteBishop; whiteQueen; whiteKing; whiteBishop; emptySquare; whiteRook|]|]

[<Fact>]
let ``Board after move without piece`` () =
    Board.init
    |> afterMove (3, 3, 4, 3)
    |> should equal None

[<Fact>]
let ``Board after move which captures same color`` () =
    Board.init
    |> afterMove (0, 7, 1, 7)
    |> should equal None
