module Tests

open System
open Xunit
open BlindfoldChessMechanics

[<Fact>]
let ``My test`` () =
    Assert.Equal(
        Say.hello("world"),
        "Hello world"
    )
