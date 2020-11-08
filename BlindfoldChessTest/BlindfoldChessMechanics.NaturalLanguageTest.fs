module BlindfoldChessMechanics.NaturalLanguageTest

open Xunit
open FsUnit.Xunit

open BlindfoldChessMechanics.NaturalLanguage

[<Fact>]
let ``O-O phrase`` () =
    phrase "O-O"
    |> should equal
        "castle kingside"

[<Fact>]
let ``O-O-O+ phrase`` () =
    phrase "O-O-O+"
    |> should equal
       "castle queenside check"

[<Fact>]
let ``O-O#`` () =
    phrase "O-O#"
    |> should equal
       "castle kingside mate"

[<Fact>]
let ``Ra4`` () =
    phrase "Rh4"
    |> should equal
       "rook h 4"

[<Fact>]
let ``Pe6`` () =
    phrase "Pe6"
    |> should equal
       "pawn e 6"

[<Fact>]
let ``♔c5`` () =
    phrase "♔c5"
    |> should equal
       "king c 5"

[<Fact>]
let ``♟️b3`` () =
    phrase "♟️b3"
    |> should equal
       "pawn b 3"

[<Fact>]
let ``♝b7`` () =
    phrase "♝b7"
    |> should equal
       "bishop b 7"

[<Fact>]
let ``Qxa1#`` () =
    phrase "Qxa1#"
    |> should equal
       "queen takes a 1 mate"

[<Fact>]
let ``cxd1=N+`` () =
    phrase "cxd1=N+"
    |> should equal
       "c takes d 1 promotes to knight check"