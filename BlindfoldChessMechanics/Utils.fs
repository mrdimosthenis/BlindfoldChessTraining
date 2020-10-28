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

let lazInfinite: int LazyList =
    LazyList.unfold (fun i -> Some (i, i + 1)) 0

let lazIndexed<'a> (laz: 'a LazyList): (int * 'a) LazyList =
    LazyList.zip lazInfinite laz

let updatedLaz<'a> (index: int) (newItem: 'a) (laz: 'a LazyList): 'a LazyList =
    laz
    |> lazIndexed
    |> LazyList.map (fun (i, item) -> if i = index then newItem else item)

let lazItem<'a> (index: int) (laz: 'a LazyList): 'a =
    laz
    |> lazIndexed
    |> LazyList.find (fun (i, _) -> i = index)
    |> snd

let updatedLazyLists<'a> (indices: int * int) (newItem: 'a) (laz: 'a LazyList LazyList): 'a LazyList LazyList =
    let (i, j) = indices
    let updatedInnerSeq = lazItem i laz |> updatedLaz j newItem
    updatedLaz i updatedInnerSeq laz

let prependedLaz<'a> (a: 'a) (laz: 'a LazyList): 'a LazyList =
    LazyList.append (LazyList.ofList [ a ]) laz

let lazExists<'a> (pred: 'a -> bool) (laz: 'a LazyList): bool =
    match LazyList.tryFind pred laz with
    | Some _ -> true
    | None -> false

let lazForAll<'a> (pred: 'a -> bool) (laz: 'a LazyList): bool =
    laz
    |> lazExists (fun x -> x |> pred |> not)
    |> not

let lazDistinct<'a when 'a: equality> (laz: 'a LazyList): 'a LazyList =
    laz
    |> LazyList.fold
            (fun acc x ->
                if lazExists ((=) x) acc then acc
                else prependedLaz x acc
            )
            LazyList.empty
    |> LazyList.rev
