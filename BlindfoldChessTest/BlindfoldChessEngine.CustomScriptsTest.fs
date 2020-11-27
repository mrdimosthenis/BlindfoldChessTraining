module BlindfoldChessEngine.CustomScriptsTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessEngine.CustomScripts
open Npgsql

[<Fact>]
let ``get one`` () =
    InsertEvaluatedLines.getOne()
    |> should equal "1"

//[<Fact>]
//let ``insert evaluated lines`` () =
//    InsertEvaluatedLines.insert()
//    |> should equal ()

//[<Fact>]
//let ``get next unparsed evaluated line id`` () =
//    Utils.withDBTransaction InsertPositions.getNextUnparsedEvaluatedLineId
//    |> Option.isSome
//    |> should equal true

//[<Fact>]
//let ``get evaluated line`` () =
//    1
//    |> InsertPositions.getEvaluatedLine
//    |> Utils.withDBTransaction
//    |> String.length
//    |> should equal 3795

//[<Fact>]
//let ``get next unparsed evaluated line and insert fens`` () =
//    InsertPositions.getNextUnparsedEvaluatedLineAndInsertFens()
//    |> should equal ()

//[<Fact>]
//let ``insert all fens`` () =
//    InsertPositions.insertAllFens()
//    |> should equal false
