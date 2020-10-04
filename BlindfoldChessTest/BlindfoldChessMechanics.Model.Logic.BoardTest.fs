﻿module BlindfoldChessMechanics.Model.Logic.BoardTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Model.Logic.Board

let emptyBoard =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays

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
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByRook (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 2); (6, 2); (5, 2)
          (1, 2); (2, 2); (3, 2)
          (4, 5); (4, 4); (4, 3)
          (4, 0); (4, 1)|]

[<Fact>]
let ``Indices cotrolled by ghost rook`` () =
    (fun () -> indicesControlledByRook (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by bishop`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByBishop (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 5); (6, 4); (5, 3)
          (0, 6); (1, 5); (2, 4); (3, 3)
          (5, 1)
          (3, 1)|]

[<Fact>]
let ``Indices cotrolled by ghost bishop`` () =
    (fun () -> indicesControlledByBishop (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by queen`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackKnight; emptySquare; emptySquare; emptySquare; blackQueen;  emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
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

[<Fact>]
let ``Indices cotrolled by ghost queen`` () =
    (fun () -> indicesControlledByQueen (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by knight in the middle`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByKnight (2, 5)
    |> Seq.toArray
    |> should equal
        [|(0, 4); (1, 3); (3, 3); (0, 6); (1, 7); (4, 6); (3, 7)|]

[<Fact>]
let ``Indices cotrolled by knight in the corner`` () =
    [|[|emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whiteKnight; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByKnight (6, 0)
    |> Seq.toArray
    |> should equal
        [|(5, 2); (7, 2)|]

[<Fact>]
let ``Indices cotrolled by ghost knight`` () =
    (fun () -> indicesControlledByKnight (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by king in the middle`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; whiteRook;   whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByKing (2, 3)
    |> Seq.toArray
    |> should equal
        [|(1, 2); (1, 3); (1, 4); (2, 4); (3, 2); (3, 3); (3, 4)|]

[<Fact>]
let ``Indices cotrolled by king in the corner`` () =
    [|[|blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whiteQueen;  blackBishop; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByKing (7, 0)
    |> Seq.toArray
    |> should equal
        [|(6, 0); (7, 1)|]

[<Fact>]
let ``Indices cotrolled by ghost king`` () =
    (fun () -> indicesControlledByKing (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by white init pawn in the edge`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackBishop; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whitePawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByPawn (1, 7)
    |> Seq.toArray
    |> should equal
        [|(2, 7); (3, 7); (2, 6)|]

[<Fact>]
let ``Indices cotrolled by white init blocked pawn`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; blackBishop; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByPawn (1, 3)
    |> Seq.toArray
    |> should equal
        [|(2, 3)|]

[<Fact>]
let ``Indices cotrolled by black blocked pawn`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; whiteRook;   whiteRook;   whiteRook;   emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByPawn (4, 3)
    |> Seq.toArray
    |> should equal
        [|(3, 2); (3, 4)|]

[<Fact>]
let ``Indices cotrolled by black init pawn in the edge`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByPawn (6, 0)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``Indices cotrolled by ghost pawn`` () =
    (fun () -> indicesControlledByKing (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Indices cotrolled by white color`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whiteKing;   blackQueen;  emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; whiteBishop; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackPawn;   emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
      [|blackPawn;   emptySquare; blackPawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByColor true
    |> Seq.toArray
    |> should equal
        [| (3, 4)
           (7, 7); (6, 6); (5, 5); (4, 4); (5, 1); (4, 2); (0, 0); (1, 1); (2, 2)
           (6, 1); (7, 0); (7, 1) |]

[<Fact>]
let ``Indices cotrolled by black color`` () =
    [|[|blackRook;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; blackKing;   emptySquare|]
      [|emptySquare; blackPawn;   emptySquare; emptySquare; blackQueen;  blackPawn;   blackPawn;   emptySquare|]
      [|blackPawn;   emptySquare; emptySquare; emptySquare; blackPawn;   emptySquare; blackBishop; blackPawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; whitePawn;   emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   whitePawn;   blackPawn;   whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; whiteQueen;  emptySquare; whitePawn;   whitePawn;   whitePawn|]
      [|emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare; emptySquare; whiteKing;   emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> indicesControlledByColor false
    |> Seq.toArray
    |> should equal
        [| (2, 2)
           (4, 0)
           (6, 7); (4, 7); (0, 1); (1, 2); (2, 3); (3, 4); (4, 5)
           (5, 1); (4, 1)
           (7, 4); (6, 2); (6, 3); (7, 5); (3, 7); (4, 6); (5, 5); (7, 3); (3, 1); (4, 2); (5, 3)
           (6, 0); (7, 2); (7, 1)
           (7, 7) |]

[<Fact>]
let ``Is white king in danger`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whiteKing;   emptySquare; whitePawn;   emptySquare; blackRook;   emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> isKingInDanger true
    |> should equal
        false

[<Fact>]
let ``Is black king in danger`` () =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; whiteKing;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; blackKing;   emptySquare; emptySquare; emptySquare; whiteRook;   emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Utils.seqOfArrays
    |> Seq.rev
    |> isKingInDanger false
    |> should equal
        true
