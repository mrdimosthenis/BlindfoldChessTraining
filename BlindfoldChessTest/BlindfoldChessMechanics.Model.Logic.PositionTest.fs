module BlindfoldChessMechanics.Model.Logic.PositionTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.Model.Logic.Position

//[<Fact>]
//let ``Board after valid pawn move`` () =
//    Board.init
//    |> afterMove (1, 0, 3, 0)
//    |> Seq.rev
//    |> toArrays
//    |> should equal
//        [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
//          [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|whitePawn;   emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
//          [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
//
//[<Fact>]
//let ``Board after valid knight move`` () =
//    Board.init
//    |> afterMove (0, 6, 2, 5)
//    |> Seq.rev
//    |> toArrays
//    |> should equal
//        [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
//          [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
//          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; whiteKnight; emptySquare; emptySquare|]
//          [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
//          [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; emptySquare; whiteRook|]|]
//
//[<Fact>]
//let ``Board after move without piece`` () =
//    (fun () -> afterMove (3, 3, 4, 3) Board.init |> ignore)
//    |> should throw typeof<InvalidMove>
//
//[<Fact>]
//let ``Board after move which captures same color`` () =
//    (fun () -> afterMove (0, 7, 1, 7) Board.init |> ignore)
//    |> should throw typeof<InvalidMove>
