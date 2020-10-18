module BlindfoldChessMechanics.Notation.Emitter

open BlindfoldChessMechanics.Logic

open System
open System.IO

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

let moveText (isWhite: bool) (areFigures: bool) (m: Position.Move): string =
    match (m.Piece, snd m.FromCoords, snd m.ToCoords) with
    | (Board.King, 4, 6) -> "O-O"
    | (Board.King, 4, 2) -> "O-O-O"
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

let rowFEN (row: Board.Resident seq): string =
    let remaining (opt: int option): string =
        match opt with
        | Some i -> string i
        | None -> ""
    let (lastAccStr, lastAccNumOpt) =
        Seq.fold (fun (accStr, accNumOpt) resident ->
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
                |> Seq.rev
                |> Seq.map rowFEN
                |> String.concat "/"
    let color = if position.IsWhiteToMove then "w"
                else "b"
    let castling = [| (position.Castling.WhiteKingSideCastle, "K")
                      (position.Castling.WhiteQueenSideCastle, "Q")
                      (position.Castling.BlackKingSideCastle, "k")
                      (position.Castling.BlackQueenSideCastle, "q") |]
                   |> Seq.ofArray
                   |> Seq.filter fst
                   |> Seq.map snd
                   |> String.concat ""
                   |> (fun s -> if s = "" then "-" else s)
    let enPassant = match position.EnPassant with
                    | Some coords -> coordinatesText coords
                    | None -> "-"
    let halfMove = string position.Halfmove
    let fullMove = string position.Fullmove
    sprintf "%s %s %s %s %s %s" board color castling enPassant halfMove fullMove

let metaTagsText(fen: string, metaTags: Map<string,string>): string =
    let fenKVs = if fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" then Array.empty
                 else [| ("FEN", fen) |]
                 |> Seq.ofArray
    let lines = metaTags
                |> Map.toSeq
                |> Seq.append fenKVs
                |> Seq.map (fun (k, v) ->
                             sprintf """[%s "%s"]""" k v
                           )
    String.Join("\n", lines)

let gameText (game: Game.Game): string =
    let fen = positionText(game.InitialPosition)
    let metaTags = metaTagsText(fen, game.MetaTags)
    let isWhiteToMove = game.InitialPosition.IsWhiteToMove
    let indices = Seq.initInfinite (fun i -> [| i + 1; i + 1 |])
                  |> Seq.map Seq.ofArray
                  |> Seq.concat
                  |> (if isWhiteToMove then id else Seq.tail)
    let isWhiteToMoveBools = Seq.initInfinite (fun _ ->
                                                if isWhiteToMove then [| true; false |]
                                                else [| false; true |]
                                              )
                             |> Seq.map Seq.ofArray
                             |> Seq.concat
    let moves = Seq.map3 (fun i b m ->
                            let d = match (b, i) with
                                    | (true, _) -> sprintf "%i. " i
                                    | (false, 1) -> sprintf "%i... " i
                                    | (false, _) -> ""
                            let m = moveText true false m
                            d + m
                         )
                         indices
                         isWhiteToMoveBools
                         game.Moves
    let result = match game.Result with
                 | None -> ""
                 | Some Game.White -> "1-0"
                 | Some Game.Black -> "0-1"
                 | Some Game.Draw -> "1/2-1/2"
    sprintf "%s\n\n%s %s"
            metaTags
            (String.Join(" ", moves))
            result
