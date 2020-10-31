module BlindfoldChessMechanics.Notation.Parser

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics

open System.Text.RegularExpressions
open System.IO
open FSharpx.Collections
open System.Text.Json

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
    |> Map.ofSeq
    
let textOfMovesWithResult (text: string): string LazyList * Game.NotedResult option =
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
                | Some _ -> LazyList.tail movesAndResultRev
                | _ -> movesAndResultRev
                |> LazyList.rev
    (moves, result)

let fenRow(row: string): Board.Resident array =
    row
    |> LazyList.ofSeq
    |> LazyList.rev
    |> LazyList.fold
            (fun acc c ->
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
                    Array.append residents acc
            )
            [||]

let textOfFen (text: string): Position.Position =
    let fenParts = text.Split(' ')
    let board = fenParts.[0].Split('/')
                |> Array.map fenRow
                |> Array.rev
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
            |> LazyList.fold
                    (fun (accPos, accMvs) m ->
                            let validMovesWithResPos = Position.movesWithResultedPosition accPos
                            match LazyList.tryFind (fun (vm, _) -> Emitter.moveText true false vm = m) validMovesWithResPos with
                            | Some (vm, resPos) ->
                                let nextAccPos = resPos
                                let nextAccMvs = Utils.prependedLaz vm accMvs
                                (nextAccPos, nextAccMvs)
                            | None ->
                                raise (InvalidMove m)
                    )
                    (initialPosition, LazyList.empty)
            |> snd
            |> LazyList.rev
            |> LazyList.toArray
    { MetaTags = metaTags
      InitialPosition = initialPosition
      Moves = validatedMoves
      Result = result }

let fileOfGameTexts(filePath: string): Game.Game LazyList =
    filePath
    |> File.ReadLines
    |> LazyList.ofSeq
    |> LazyList.scan
            (fun (accLines, prevLine, _) s ->
                    match (prevLine, s.StartsWith("[")) with
                    | ("", true) -> ("", s, Some accLines)
                    | _ -> (accLines + "\n" + s, s, None)
            )
            ("", "", None)
    |> LazyList.rev
    |> Utils.lazIndexed
    |> LazyList.map
            (fun (i, (accLines, _, accGameStrOpt)) ->
                     match (i, accGameStrOpt) with
                     | (0, _) -> LazyList.ofList [ accLines ]
                     | (_, Some s) -> LazyList.ofList [ s ]
                     | _ -> LazyList.empty
            )
    |> LazyList.concat
    |> LazyList.rev
    |> LazyList.filter (fun s -> s.Replace("\n", "").Trim() <> "")
    |> LazyList.map textOfGame

let jsonOfGame (json: string): Game.Game =
    JsonSerializer.Deserialize(json, Game.jsonOptions)

let fileOfGameJsons(filePath: string): Game.Game LazyList =
    filePath
    |> File.ReadLines
    |> LazyList.ofSeq
    |> LazyList.map jsonOfGame
