module BlindfoldChessMechanics.Utils

open FSharpx.Collections

// LazyList

let lazyListSingleton<'a> (a: 'a): LazyList<'a> =
    LazyList.ofList [ a ]

let lazyListInfiniteIndices(): LazyList<int> =
    Seq.initInfinite id
    |> LazyList.ofSeq

let lazyListIndexed<'a> (llst: LazyList<'a>): LazyList<int * 'a> =
    lazyListInfiniteIndices()
    |> LazyList.zip llst
    |> LazyList.map (fun (e, i) -> (i, e))

let lazyListInit<'a> (n: int) (f: int -> 'a): LazyList<'a> =
    lazyListInfiniteIndices()
    |> LazyList.map f
    |> LazyList.take n

let lazyListDistinct<'a when 'a : equality> (llst: LazyList<'a>): LazyList<'a> =
    llst
    |> LazyList.toSeq
    |> Seq.distinct
    |> LazyList.ofSeq

let lazyListForAll<'a> (pred: 'a -> bool) (llst: LazyList<'a>): bool =
    llst
    |> LazyList.tryFind (fun e -> e |> pred |> not)
    |> Option.isNone

let lazyListOfMap<'a, 'b when 'a: comparison> (m: Map<'a, 'b>): LazyList<'a * 'b> =
    m
    |> Map.toSeq
    |> LazyList.ofSeq

let lazyListToMap<'a, 'b when 'a: comparison> (lst: LazyList<'a * 'b>): Map<'a, 'b> =
    lst
    |> LazyList.toSeq
    |> Map.ofSeq

let lazyListItem<'a> (index: int) (llst: LazyList<'a>): 'a =
    llst
    |> lazyListIndexed
    |> LazyList.tryFind (fun (i, _) -> i = index)
    |> Option.get
    |> snd

let seqToArrays<'a> (llst: LazyList<LazyList<'a>>): 'a array array =
    llst
    |> LazyList.map LazyList.toArray
    |> LazyList.toArray

let seqOfArrays<'a> (arr: 'a array array): LazyList<LazyList<'a>> =
    arr |> LazyList.ofArray |> LazyList.map LazyList.ofArray

let updatedLazyList<'a> (index: int) (newItem: 'a) (llst: LazyList<'a>): LazyList<'a> =
    llst
    |> lazyListIndexed
    |> LazyList.map (fun (i, item) -> if i = index then newItem else item)

let updatedLazyListuences<'a> (indices: int * int) (newItem: 'a) (llst: LazyList<LazyList<'a>>): LazyList<LazyList<'a>> =
    let (i, j) = indices
    let updatedInnerLazyList = lazyListItem i llst |> updatedLazyList j newItem
    updatedLazyList i updatedInnerLazyList llst

let prependedLazyList<'a> (a: 'a) (llst: LazyList<'a>): LazyList<'a> =
    LazyList.append (lazyListSingleton a) llst
