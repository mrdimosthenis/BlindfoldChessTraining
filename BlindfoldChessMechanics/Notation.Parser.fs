module BlindfoldChessMechanics.Notation.Parser

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics

open System.Text.RegularExpressions
open System.IO
open FSharpx.Collections

exception InvalidMove of string

// functions

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
    | _ -> raise (Emitter.InvalidColumn c)

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
    |> LazyList.ofSeq
    |> Utils.lazyListToMap
    
let textOfMovesWithResult (text: string): LazyList<string> * Game.NotedResult option =
    let justMovesAndResult = Regex.Replace(text, "\([^)]+\)|\[[^\]]+\]|\{[^}]+\}|\d+\.+", "")
    let movesAndResultRev = Regex.Split(justMovesAndResult, "\s+")
                             |> LazyList.ofArray
                             |> LazyList.filter ((<>) "")
                             |> LazyList.rev
    let result = match LazyList.head movesAndResultRev with
                 | "1-0" -> Some Game.White
                 | "0-1" -> Some Game.Black
                 | "1/2-1/2" -> Some Game.Draw
                 | _ -> None
    let moves = match result with
                | Some _ -> movesAndResultRev |> LazyList.tail |> LazyList.rev
                | _ -> movesAndResultRev |> LazyList.rev
    (moves, result)

let fenRow(row: string): LazyList<Board.Resident> =
    row.ToCharArray()
    |> LazyList.ofArray
    |> LazyList.fold (fun acc c ->
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
                        LazyList.append (LazyList.ofArray residents) acc
                     )
                     LazyList.empty
    |> LazyList.rev

let textOfFen (text: string): Position.Position =
    let fenParts = text.Split(' ')
    let board = fenParts.[0].Split('/')
                |> LazyList.ofArray
                |> LazyList.map fenRow
                |> LazyList.rev
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

let textOfGame (text: string): Game.Game =
    let metaTags = textOfMetaTags text
    let initialPosition = metaTags.TryFind("FEN")
                          |> Option.map textOfFen
                          |> Option.defaultValue Position.init
    let (moves, result) = textOfMovesWithResult text
    let validatedMoves =
            moves
            |> LazyList.fold (fun (accPos, accMvs) m ->
                            let validMoves = Position.moves accPos
                            match LazyList.tryFind (fun vm -> Emitter.moveText true false vm = m) validMoves with
                            | Some vm ->
                                let nextAccPos = Position.positionAfterMove vm accPos
                                let nextAccMvs = Utils.prependedLazyList vm accMvs
                                (nextAccPos, nextAccMvs)
                            | None ->
                                raise (InvalidMove m)
                        )
                        (initialPosition, LazyList.empty)
            |> snd
            |> LazyList.rev
    { MetaTags = metaTags
      InitialPosition = initialPosition
      Moves = validatedMoves
      Result = result }
