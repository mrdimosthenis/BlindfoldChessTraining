module BlindfoldChessEngine.CustomScriptsTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessEngine.CustomScripts
open Npgsql
open FSharpx.Collections

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

[<Fact>]
let ``parse evals`` () =
    """1. Nf3 { [%eval 0.2] } 1... Nh6? { [%eval 1.37] }
    2. Nc3 { [%eval 0.98] } 2... Ng4?! { [%eval 1.56] }
    3. Ne4? { [%eval -0.64] } 3... Ne5?? { [%eval 4.99] }
    4. h3?? { [%eval -0.38] } 4... Ng4?? { [%eval 4.84] }
    5. g3?? { [%eval 0.35] } 5... Nxf2?? { [%eval 3.93] }
    6. Rh2?? { [%eval -10.35] } 6... Nxh3?? { [%eval 2.41] }
    7. Rh1?? { [%eval -2.9] } 7... Nf4?? { [%eval 3.12] }
    8. Rg1?? { [%eval -2.38] } 8... Nxe2?? { [%eval 1.82] }
    9. Rh1?? { [%eval -4.42] } 9... Nxg3?? { [%eval 0.16] }
    10. Rg1?? { [%eval -9.1] } 10... Nxf1?? { [%eval -5.84] }
    11. Rxf1 { [%eval -6.07] } 11... Nc6# 0-1"""
    |> InsertEvals.parseEvals
    |> Seq.toList
    |> should equal
                [ "0.2"; "1.37"
                  "0.98"; "1.56"
                  "-0.64"; "4.99"
                  "-0.38"; "4.84"
                  "0.35"; "3.93"
                  "-10.35"; "2.41"
                  "-2.9"; "3.12"
                  "-2.38"; "1.82"
                  "-4.42"; "0.16"
                  "-9.1"; "-5.84"
                  "-6.07" ]

//[<Fact>]
//let ``insert all fens`` () =
//    InsertEvals.insertAllEvals()
//    |> should equal false

//[<Fact>]
//let ``neural network json`` () =
//    Train.newNeuralNetworkJson()
//    |> should equal ""

//[<Fact>]
//let ``infinite fit`` () =
//    Train.infiniteFit()
//    |> should equal ()

//[<Fact>]
//let ``infinite create fit and insert net`` () =
//    RandomNet.infiniteCreateFitAndInsertNet()
//    |> should equal ()

[<Fact>]
let ``train from data points`` () =
    TrainWithCsv.trainFromDataPoints()
    |> should equal ()
