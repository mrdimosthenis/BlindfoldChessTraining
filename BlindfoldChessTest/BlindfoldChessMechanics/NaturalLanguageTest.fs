module BlindfoldChessTest.BlindfoldChessMechanics.NaturalLanguageTest

open BlindfoldChessMechanics.NaturalLanguage
open NUnit.Framework

[<Test>]
let ``O-O phrase`` () =
    Assert.AreEqual("castle kingside", phrase "O-O")

[<Test>]
let ``O-O-O+ phrase`` () =
    Assert.AreEqual("castle queenside check", phrase "O-O-O+")

[<Test>]
let ``O-O#`` () =
    Assert.AreEqual("castle kingside mate", phrase "O-O#")

[<Test>]
let ``Ra4`` () =
    Assert.AreEqual("rook H four", phrase "Rh4")

[<Test>]
let ``Pe6`` () =
    Assert.AreEqual("pawn E six", phrase "Pe6")

[<Test>]
let ``♔c5`` () =
    Assert.AreEqual("king C five", phrase "♔c5")

[<Test>]
let ``♟️b3`` () =
    Assert.AreEqual("pawn B three", phrase "♟️b3")

[<Test>]
let ``♝b7`` () =
    Assert.AreEqual("bishop B seven", phrase "♝b7")

[<Test>]
let ``Qxa1#`` () =
    Assert.AreEqual("queen takes A one mate", phrase "Qxa1#")

[<Test>]
let ``cxd1=N+`` () =
    Assert.AreEqual("C takes D one promotes to knight check", phrase "cxd1=N+")
    