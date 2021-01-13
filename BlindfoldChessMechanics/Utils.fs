module BlindfoldChessMechanics.Utils

open FSharpx.Collections

let updatedArrays<'a> (indices: int * int) (newItem: 'a) (arr: 'a array array): 'a array array =
    let (i, j) = indices
    let newInnerArr = Array.copy arr.[i]
    newInnerArr.[j] <- newItem
    let newArr = Array.copy arr
    newArr.[i] <- newInnerArr
    newArr

let lazToArrays<'a> (laz: 'a LazyList LazyList): 'a array array = laz |> LazyList.map LazyList.toArray |> LazyList.toArray

let lazOfArrays<'a> (arr: 'a array array): 'a LazyList LazyList =
    arr
    |> LazyList.ofArray
    |> LazyList.map LazyList.ofArray

let laz2DIndices (m: int) (n: int): (int * int) LazyList =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take m
    |> LazyList.map
            (fun i ->
                    id
                    |> Seq.initInfinite
                    |> LazyList.ofSeq
                    |> LazyList.take n
                    |> LazyList.map (fun j -> (i, j))
            )
    |> LazyList.concat

let updatedLaz<'a> (index: int) (newItem: 'a) (laz: 'a LazyList): 'a LazyList =
    laz
    |> Seq.indexed
    |> LazyList.ofSeq
    |> LazyList.map (fun (i, item) -> if i = index then newItem else item)

let updatedLazyLists<'a> (indices: int * int) (newItem: 'a) (laz: 'a LazyList LazyList): 'a LazyList LazyList =
    let (i, j) = indices
    let updatedInnerSeq = Seq.item i laz |> updatedLaz j newItem
    updatedLaz i updatedInnerSeq laz

let prependedLaz<'a> (a: 'a) (laz: 'a LazyList): 'a LazyList =
    LazyList.append (LazyList.ofList [ a ]) laz
