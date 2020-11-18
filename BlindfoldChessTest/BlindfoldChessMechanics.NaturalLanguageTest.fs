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
       "rook H four"

[<Fact>]
let ``Pe6`` () =
    phrase "Pe6"
    |> should equal
       "pawn E six"

[<Fact>]
let ``♔c5`` () =
    phrase "♔c5"
    |> should equal
       "king C five"

[<Fact>]
let ``♟️b3`` () =
    phrase "♟️b3"
    |> should equal
       "pawn B three"

[<Fact>]
let ``♝b7`` () =
    phrase "♝b7"
    |> should equal
       "bishop B seven"

[<Fact>]
let ``Qxa1#`` () =
    phrase "Qxa1#"
    |> should equal
       "queen takes A one mate"

[<Fact>]
let ``cxd1=N+`` () =
    phrase "cxd1=N+"
    |> should equal
       "C takes D one promotes to knight check"