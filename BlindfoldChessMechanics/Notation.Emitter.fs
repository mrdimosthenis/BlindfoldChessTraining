module BlindfoldChessMechanics.Notation.Emitter

open BlindfoldChessMechanics.Logic

open System
open System.IO
open FSharpx.Collections
open BlindfoldChessMechanics
open System.Text.Json

exception InvalidColumn of string

// functions

let rowText(r: int): string =
    string (r + 1)

let columnText(c: int): string =
    match c with
    | 0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | 4 -> "e"
    | 5 -> "f"
    | 6 -> "g"
    | 7 -> "h"
    | _ -> c |> string |> InvalidColumn |> raise

let coordinatesText (coords: Board.Coordinates): string =
    let (r, c) = coords
    columnText c + rowText r

let pieceText (isWhite: bool) (areFigures: bool) (p: Board.Piece): string =
    match (p, areFigures, isWhite) with
    | (Board.King, false, true) -> "K"
    | (Board.Queen, false, true) -> "Q"
    | (Board.Rook, false, true) -> "R"
    | (Board.Bishop, false, true) -> "B"
    | (Board.Knight, false, true) -> "N"
    | (Board.Pawn, false, true) -> "P"
    | (Board.King, false, false) -> "k"
    | (Board.Queen, false, false) -> "q"
    | (Board.Rook, false, false) -> "r"
    | (Board.Bishop, false, false) -> "b"
    | (Board.Knight, false, false) -> "n"
    | (Board.Pawn, false, false) -> "p"
    | (Board.King, true, true) -> "♔"
    | (Board.Queen, true, true) -> "♕"
    | (Board.Rook, true, true) -> "♖"
    | (Board.Bishop, true, true) -> "♗"
    | (Board.Knight, true, true) -> "♘"
    | (Board.Pawn, true, true) -> "♙"
    | (Board.King, true, false) -> "♚"
    | (Board.Queen, true, false) -> "♛"
    | (Board.Rook, true, false) -> "♜"
    | (Board.Bishop, true, false) -> "♝"
    | (Board.Knight, true, false) -> "♞"
    | (Board.Pawn, true, false) -> "♟︎"

let textsOfPieces (areFigures: bool) (board: Board.Board): string LazyList * string LazyList =
    let piecesWithColorAndPriorityGroups =
            Utils.laz2DIndices 8 8
            |> LazyList.map
                  (fun (i, j) ->
                      match Board.resident (i, j) board with
                      | Some piece ->
                          let pieceDescription =
                                piece.PieceType
                                |> pieceText piece.IsWhite areFigures
                                |> String.map Char.ToUpper
                          let description = pieceDescription + columnText j + rowText i
                          let priority = match piece.PieceType with
                                         | Board.King -> 1
                                         | Board.Pawn -> 2
                                         | Board.Knight -> 3
                                         | Board.Bishop -> 4
                                         | Board.Rook -> 5
                                         | Board.Queen -> 6
                          [ (description, piece.IsWhite, priority) ]
                      | None ->
                          []
                      |> LazyList.ofList
                  )
            |> LazyList.concat
            |> Seq.sortBy (fun (_, _, p) -> p)
            |> Seq.groupBy (fun (_, w, _) -> w)
            |> LazyList.ofSeq
            |> LazyList.map (fun (k, s) -> (k, LazyList.ofSeq s))
    let whitePieces = piecesWithColorAndPriorityGroups
                      |> LazyList.find fst
                      |> snd
                      |> LazyList.map (fun (d, _, _) -> d)
    let blackPieces = piecesWithColorAndPriorityGroups
                      |> LazyList.find (fun t -> t |> fst |> not)
                      |> snd
                      |> LazyList.map (fun (d, _, _) -> d)
    (whitePieces, blackPieces)

let moveText (isWhite: bool) (areFigures: bool) (m: Position.Move): string =
    match (m.Piece, snd m.FromCoords, snd m.ToCoords, m.IsCheck, m.IsMate) with
    | (Board.King, 4, 6, false, false) -> "O-O"
    | (Board.King, 4, 2, false, false) -> "O-O-O"
    | (Board.King, 4, 6, true, false) -> "O-O+"
    | (Board.King, 4, 2, true, false) -> "O-O-O+"
    | (Board.King, 4, 6, true, true) -> "O-O#"
    | (Board.King, 4, 2, true, true) -> "O-O-O#"
    | _ -> let piece = match m.Piece with
                       | Board.Pawn -> ""
                       | _ -> m.Piece
                              |> pieceText isWhite areFigures
                              |> String.map Char.ToUpper
           let clarification =
               match (m.Piece, m.SamePieceCoords, m.IsCapture) with
               | (Board.Pawn, _, true) -> m.FromCoords
                                          |> snd
                                          |> columnText
               | (Board.Pawn, _, false) -> ""
               | (_, Some (_, c), _) when c = (snd m.FromCoords) -> m.FromCoords
                                                                    |> fst
                                                                    |> rowText
               | (_, Some _, _) -> m.FromCoords
                                   |> snd
                                   |> columnText
               | _ -> ""
           let takes = if m.IsCapture then "x" else ""
           let targetSquare = coordinatesText m.ToCoords
           let promotion = match m.Promotion with
                           | Some p -> pieceText isWhite areFigures p
                                       |> String.map Char.ToUpper
                                       |> (+) "="
                           | None -> ""
           let checkOrMate = match (m.IsCheck, m.IsMate) with
                             | (_, true) -> "#"
                             | (true, _) -> "+"
                             | _ -> ""
           piece + clarification + takes + targetSquare + promotion + checkOrMate

let moveTextsWithNumberIndicators  (areFigures: bool) (isWhiteToMove: bool) (moves: Position.Move LazyList): (string * bool) LazyList =
    let indices = Utils.lazInfinite
                  |> LazyList.map (fun i -> [ i + 1; i + 1 ])
                  |> LazyList.map LazyList.ofList
                  |> LazyList.concat
                  |> (if isWhiteToMove then id else LazyList.tail)
    let isWhiteToMoveBools =
        Utils.lazInfinite
        |> LazyList.map (fun _ ->
             if isWhiteToMove then [ true; false ]
             else [ false; true ]
          )
        |> LazyList.map LazyList.ofList
        |> LazyList.concat
    LazyList.zip indices isWhiteToMoveBools
    |> LazyList.zip moves
    |> LazyList.map
        (fun (m, (i, b)) ->
            let numberIndicators = match (b, i, isWhiteToMove) with
                                   | (true, _, _) -> [ (sprintf "%i." i, true) ]
                                   | (false, 1, false) -> [ (sprintf "%i..." i, true) ]
                                   | (false, _, _) -> []
                                   |> LazyList.ofList
            [ (moveText b areFigures m, false) ]
            |> LazyList.ofList
            |> LazyList.append numberIndicators
        )
    |> LazyList.concat

let multipleMovesText (areFigures: bool) (isWhiteToMove: bool) (moves: Position.Move LazyList): string =
    let textsWithNumbers = moveTextsWithNumberIndicators areFigures isWhiteToMove moves
    let (head, tail) = LazyList.uncons textsWithNumbers
    LazyList.zip tail textsWithNumbers
    |> LazyList.map (fun ((s, _), (_, prevIndicator)) -> (s, prevIndicator))
    |> Utils.prependedLaz head
    |> LazyList.fold
            (fun acc (s, prevIndicator) ->
                if prevIndicator then acc + s
                else acc + " " + s
            )
            ""

let rowFEN (row: Board.Resident array): string =
    let remaining (opt: int option): string =
        match opt with
        | Some i -> string i
        | None -> ""
    let (lastAccStr, lastAccNumOpt) =
        Array.fold
                (fun (accStr, accNumOpt) resident ->
                       match (resident, accNumOpt) with
                       | (None, None) ->
                            (accStr, Some 1)
                       | (None, Some i) ->
                            (accStr, Some (i + 1))
                       | (Some p: Board.Resident, _) ->
                            (accStr + remaining accNumOpt + pieceText p.IsWhite false p.PieceType, None)
                )
                ("", None)
                row
    lastAccStr + remaining lastAccNumOpt

let positionText (position: Position.Position): string =
    let board = position.Board
                |> Array.rev
                |> Array.map rowFEN
                |> String.concat "/"
    let color = if position.IsWhiteToMove then "w"
                else "b"
    let castling = [ (position.Castling.WhiteKingSideCastle, "K")
                     (position.Castling.WhiteQueenSideCastle, "Q")
                     (position.Castling.BlackKingSideCastle, "k")
                     (position.Castling.BlackQueenSideCastle, "q") ]
                   |> LazyList.ofList
                   |> LazyList.filter fst
                   |> LazyList.map snd
                   |> String.concat ""
                   |> (fun s -> if s = "" then "-" else s)
    let enPassant = match position.EnPassant with
                    | Some coords -> coordinatesText coords
                    | None -> "-"
    let halfMove = string position.Halfmove
    let fullMove = string position.Fullmove
    sprintf "%s %s %s %s %s %s" board color castling enPassant halfMove fullMove

let metaTagsText(fen: string, metaTags: Map<string,string>): string =
    let fenKVs = if fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" then LazyList.empty
                 else LazyList.ofList [ ("FEN", fen) ]
    let lines = metaTags
                |> Map.toSeq
                |> Seq.append fenKVs
                |> Seq.map (fun (k, v) ->
                             sprintf """[%s "%s"]""" k v
                           )
                |> Seq.cache
    String.Join("\n", lines)

let gameText (game: Game.Game): string =
    let fen = positionText(game.InitialPosition)
    let metaTags = metaTagsText(fen, game.MetaTags)
    let moves = game.Moves
                |> LazyList.ofArray
                |> multipleMovesText false game.InitialPosition.IsWhiteToMove 
    let result = match game.Result with
                 | None -> "*"
                 | Some Game.White -> "1-0"
                 | Some Game.Black -> "0-1"
                 | Some Game.Draw -> "1/2-1/2"
    sprintf "%s\n\n%s  %s"
            metaTags
            moves
            result

let gameFileTexts(filePath: string) (games: Game.Game LazyList): unit =
    let w = File.AppendText filePath
    LazyList.iter
            (fun g->
                w.WriteLine(gameText g)
                w.WriteLine()
             )
             games
    w.Close()

let gameJson (game: Game.Game): string =
    JsonSerializer.Serialize(game, Game.jsonOptions)

let gameFileJsons(filePath: string) (games: Game.Game LazyList): unit =
    let w = File.AppendText filePath
    LazyList.iter
            (fun g->
                w.WriteLine(gameJson g)
             )
             games
    w.Close()
