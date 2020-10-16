module BlindfoldChessMechanics.Notation.Parser

open BlindfoldChessMechanics.Logic

open System.Text.RegularExpressions

let textOfRow(r: string): int =
    int r - 1

let textOfColumn(c: string): int =
    match c with
    | "a" -> 0
    | "b" -> 1
    | "c" -> 2
    | "d" -> 3
    | "e" -> 4
    | "f" -> 5
    | "g" -> 6
    | "h" -> 7
    | _ -> raise Emitter.InvalidColumn

let textOfcoordinates(coords: string): Board.Coordinates =
    let r = textOfRow coords.[1..1]
    let c = textOfColumn coords.[0..0]
    (r, c)

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

let fenRow(row: string): Board.Resident seq =
    Seq.foldBack (fun c acc ->
                    let residents = match c with
                                    | 'K' -> [| Some { Board.PieceType = Board.King; Board.IsWhite = true } |]
                                    | 'Q' -> [| Some { Board.PieceType = Board.Queen; Board.IsWhite = true } |]
                                    | 'R' -> [| Some { Board.PieceType = Board.Rook; Board.IsWhite = true } |]
                                    | 'B' -> [| Some { Board.PieceType = Board.Bishop; Board.IsWhite = true } |]
                                    | 'N' -> [| Some { Board.PieceType = Board.Knight; Board.IsWhite = true } |]
                                    | 'P' -> [| Some { Board.PieceType = Board.Pawn; Board.IsWhite = true } |]
                                    | 'k' -> [| Some { Board.PieceType = Board.King; Board.IsWhite = false } |]
                                    | 'q' -> [| Some { Board.PieceType = Board.Queen; Board.IsWhite = false } |]
                                    | 'r' -> [| Some { Board.PieceType = Board.Rook; Board.IsWhite = false } |]
                                    | 'b' -> [| Some { Board.PieceType = Board.Bishop; Board.IsWhite = false } |]
                                    | 'n' -> [| Some { Board.PieceType = Board.Knight; Board.IsWhite = false } |]
                                    | 'p' -> [| Some { Board.PieceType = Board.Pawn; Board.IsWhite = false } |]
                                    | d -> Array.create (int d - int '0') None
                    Seq.append (Seq.ofArray residents) acc
                 )
                 row
                 Seq.empty

let textOfFen (text: string): Position.Position =
    let fenParts = text.Split(' ')
    let board = fenParts.[0].Split('/')
                |> Seq.ofArray
                |> Seq.map fenRow
                |> Seq.rev
    let isWhiteToMove = (fenParts.[1] = "w")
    let castling = match fenParts.[2] with
                   | "-" -> { Position.WhiteKingSideCastle = false
                              Position.WhiteQueenSideCastle = false
                              Position.BlackKingSideCastle = false
                              Position.BlackQueenSideCastle = false }
                   | castle -> { Position.WhiteKingSideCastle = castle.Contains("K")
                                 Position.WhiteQueenSideCastle = castle.Contains("Q")
                                 Position.BlackKingSideCastle = castle.Contains("k")
                                 Position.BlackQueenSideCastle = castle.Contains("q") }
    let enPassant = match fenParts.[3] with
                    | "-" -> None
                    | coords -> coords |> textOfcoordinates |> Some
    let halfMove = int fenParts.[4]
    let fullMove = int fenParts.[5]
    { Board = board
      IsWhiteToMove = isWhiteToMove
      Castling = castling
      EnPassant = enPassant
      Halfmove = halfMove
      Fullmove = fullMove }
