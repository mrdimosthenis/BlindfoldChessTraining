module BlindfoldChessMechanics.Utils

// Seq

let seqToArrays<'a> (s: 'a seq seq): 'a array array = s |> Seq.map Seq.toArray |> Seq.toArray

let seqOfArrays<'a> (arr: 'a array array): 'a seq seq =
    arr |> Seq.ofArray |> Seq.map Seq.ofArray

let updatedSeq<'a> (index: int) (newItem: 'a) (s: 'a seq): 'a seq =
    s
    |> Seq.indexed
    |> Seq.map (fun (i, item) -> if i = index then newItem else item)

let prependedSeq<'a> (a: 'a) (s: 'a seq): 'a seq = Seq.append (seq [ a ]) s
