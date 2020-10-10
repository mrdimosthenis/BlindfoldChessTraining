module BlindfoldChessMechanics.Logic.BoardTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Logic.Board

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
let ``Coordinates cotrolled by rook`` () =
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
    |> coordinatesControlledByRook (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 2); (6, 2); (5, 2)
          (1, 2); (2, 2); (3, 2)
          (4, 5); (4, 4); (4, 3)
          (4, 0); (4, 1)|]

[<Fact>]
let ``Coordinates cotrolled by ghost rook`` () =
    (fun () -> coordinatesControlledByRook (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by bishop`` () =
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
    |> coordinatesControlledByBishop (4, 2)
    |> Seq.toArray
    |> should equal
        [|(7, 5); (6, 4); (5, 3)
          (0, 6); (1, 5); (2, 4); (3, 3)
          (5, 1)
          (3, 1)|]

[<Fact>]
let ``Coordinates cotrolled by ghost bishop`` () =
    (fun () -> coordinatesControlledByBishop (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by queen`` () =
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
    |> coordinatesControlledByQueen (4, 4)
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
let ``Coordinates cotrolled by ghost queen`` () =
    (fun () -> coordinatesControlledByQueen (4, 2) emptyBoard |> Seq.toArray |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by knight in the middle`` () =
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
    |> coordinatesControlledByKnight (2, 5)
    |> Seq.toArray
    |> should equal
        [|(0, 4); (1, 3); (3, 3); (0, 6); (1, 7); (4, 6); (3, 7)|]

[<Fact>]
let ``Coordinates cotrolled by knight in the corner`` () =
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
    |> coordinatesControlledByKnight (6, 0)
    |> Seq.toArray
    |> should equal
        [|(5, 2); (7, 2)|]

[<Fact>]
let ``Coordinates cotrolled by ghost knight`` () =
    (fun () -> coordinatesControlledByKnight (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by king in the middle`` () =
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
    |> coordinatesControlledByKing (2, 3)
    |> Seq.toArray
    |> should equal
        [|(1, 2); (1, 3); (1, 4); (2, 4); (3, 2); (3, 3); (3, 4)|]

[<Fact>]
let ``Coordinates cotrolled by king in the corner`` () =
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
    |> coordinatesControlledByKing (7, 0)
    |> Seq.toArray
    |> should equal
        [|(6, 0); (7, 1)|]

[<Fact>]
let ``Coordinates cotrolled by ghost king`` () =
    (fun () -> coordinatesControlledByKing (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by white init pawn in the edge`` () =
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
    |> coordinatesControlledByPawn (1, 7)
    |> Seq.toArray
    |> should equal
        [|(2, 7); (3, 7); (2, 6)|]

[<Fact>]
let ``Coordinates cotrolled by white init blocked pawn`` () =
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
    |> coordinatesControlledByPawn (1, 3)
    |> Seq.toArray
    |> should equal
        [|(2, 3)|]

[<Fact>]
let ``Coordinates cotrolled by black blocked pawn`` () =
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
    |> coordinatesControlledByPawn (4, 3)
    |> Seq.toArray
    |> should equal
        [|(3, 2); (3, 4)|]

[<Fact>]
let ``Coordinates cotrolled by black init pawn in the edge`` () =
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
    |> coordinatesControlledByPawn (6, 0)
    |> Seq.toArray
    |> should equal
        [||]

[<Fact>]
let ``Coordinates cotrolled by ghost pawn`` () =
    (fun () -> coordinatesControlledByKing (4, 2) emptyBoard |> ignore)
    |> should throw typeof<NoPieceInBoard>

[<Fact>]
let ``Coordinates cotrolled by white color`` () =
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
    |> coordinatesControlledByColor true
    |> Seq.toArray
    |> should equal
        [| (3, 4)
           (7, 7); (6, 6); (5, 5); (4, 4); (5, 1); (4, 2); (0, 0); (1, 1); (2, 2)
           (6, 1); (7, 0); (7, 1) |]

[<Fact>]
let ``Coordinates cotrolled by black color`` () =
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
    |> coordinatesControlledByColor false
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
let ``Is white king in danger in initial board`` () =
    init
    |> isKingInDanger true
    |> should equal
        false

[<Fact>]
let ``Is black king in danger in initial board`` () =
    init
    |> isKingInDanger false
    |> should equal
        false

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
