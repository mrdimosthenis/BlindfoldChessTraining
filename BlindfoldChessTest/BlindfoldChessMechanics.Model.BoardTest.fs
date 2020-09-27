module BlindfoldChessMechanics.Model.BoardTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Model.Board

[<Fact>]
let ``Board after valid pawn move`` () =
    Board.init
    |> afterMove (1, 0, 3, 0)
    |> Seq.rev
    |> toArrays
    |> should equal
        [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
          [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
          [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]

[<Fact>]
let ``Board after valid knight move`` () =
    Board.init
    |> afterMove (0, 6, 2, 5)
    |> Seq.rev
    |> toArrays
    |> should equal
        [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
          [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
          [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
          [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; emptySquare; whiteRook|]|]

[<Fact>]
let ``Board after move without piece`` () =
    (fun () -> afterMove (3, 3, 4, 3) Board.init |> ignore)
    |> should throw typeof<InvalidMove>

[<Fact>]
let ``Board after move which captures same color`` () =
    (fun () -> afterMove (0, 7, 1, 7) Board.init |> ignore)
    |> should throw typeof<InvalidMove>

[<Fact>]
let ``Indices cotrolled by rook`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare; whiteBishop; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> ofArrays
    |> Seq.rev
    |> indicesControlledByRook (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 2); (6, 2); (5, 2)
          (1, 2); (2, 2); (3, 2)
          (4, 5); (4, 4); (4, 3)
          (4, 0); (4, 1)|]

[<Fact>]
let ``Indices cotrolled by bishop`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn; emptySquare;   emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> ofArrays
    |> Seq.rev
    |> indicesControlledByBishop (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 5); (6, 4); (5, 3)
          (0, 6); (1, 5); (2, 4); (3, 3)
          (5, 1)
          (3, 1)|]

[<Fact>]
let ``Indices cotrolled by queen`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackKnight; emptySquare; emptySquare; emptySquare; blackQueen;  emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   emptySquare;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> ofArrays
    |> Seq.rev
    |> indicesControlledByQueen (4, 4)
    |> Seq.toArray
    |> should equal
        [|(7, 4); (6, 4); (5, 4)
          (2, 4); (3, 4)
          (4, 7); (4, 6); (4, 5)
          (4, 1); (4, 2); (4, 3)
          (7, 7); (6, 6); (5, 5)
          (1, 7); (2, 6); (3, 5)
          (7, 1); (6, 2); (5, 3)
          (0, 0); (1, 1); (2, 2); (3, 3)|]

