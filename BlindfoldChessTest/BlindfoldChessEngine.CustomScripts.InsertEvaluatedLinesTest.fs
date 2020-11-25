module BlindfoldChessEngine.CustomScripts.InsertEvaluatedLinesTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessEngine.CustomScripts.InsertEvaluatedLines

[<Fact>]
let ``get one`` () =
    getOne()
    |> should equal "1"

//[<Fact>]
//let ``insert`` () =
//    insert()
//    |> should equal ()
