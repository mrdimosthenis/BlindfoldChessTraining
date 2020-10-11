module BlindfoldChessMechanics.Notation.Parse

open System.Text.RegularExpressions

let textOfMetaTags (text: string): Map<string,string> =
    let groupVal (i: int) (g: Group seq): string =
        (Seq.item i g).Value
    Regex.Matches(text, "\[(.+) \"(.+)\"\]")
    |> Seq.cast
    |> Seq.map (fun (x:Match) -> x.Groups)
    |> Seq.map Seq.cast<Group>
    |> Seq.map (fun g -> (groupVal 1 g, groupVal 2 g))
    |> Map.ofSeq
    

let textOfMoves (text: string): string seq =
    let justMoves = Regex.Replace(text, "\([^)]+\)|\[[^\]]+\]|\{[^}]+\}|\d+\.+", "")
    Regex.Split(justMoves, "\s+")
    |> Seq.ofArray
    |> Seq.filter (fun m ->
                        match m with
                        | "" | "1-0" | "0-1" | "1/2-1/2" -> false
                        | _ -> true
                  )
