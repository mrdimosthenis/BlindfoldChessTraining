module BlindfoldChessMechanics.Utils

open FSharpx.Collections

let updatedArrays (i, j) newItem (arr: 'a array array) =
    let newInnerArr = Array.copy arr[i]
    newInnerArr[j] <- newItem
    let newArr = Array.copy arr
    newArr[i] <- newInnerArr
    newArr

let lazToArrays laz =
    laz |> LazyList.map LazyList.toArray |> LazyList.toArray

let lazOfArrays arr =
    arr |> LazyList.ofArray |> LazyList.map LazyList.ofArray

let laz2DIndices m n =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take m
    |> LazyList.map (fun i ->
        id
        |> Seq.initInfinite
        |> LazyList.ofSeq
        |> LazyList.take n
        |> LazyList.map (fun j -> (i, j)))
    |> LazyList.concat

let updatedLaz index newItem laz =
    laz
    |> Seq.indexed
    |> LazyList.ofSeq
    |> LazyList.map (fun (i, item) -> if i = index then newItem else item)

let updatedLazyLists (i, j) newItem laz =
    let updatedInnerSeq = Seq.item i laz |> updatedLaz j newItem
    updatedLaz i updatedInnerSeq laz

let prependedLaz a laz =
    LazyList.append (LazyList.ofList [ a ]) laz
