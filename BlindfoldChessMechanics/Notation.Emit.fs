module BlindfoldChessMechanics.Notation.Emit

open BlindfoldChessMechanics.Logic

open System

exception InvalidColumn

// functions

let rowName(r: int): string =
    string (r + 1)

let columnName(c: int): string =
    match c with
    | 0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | 4 -> "e"
    | 5 -> "f"
    | 6 -> "g"
    | 7 -> "h"
    | _ -> raise InvalidColumn

let coordinatesName (coords: Board.Coordinates): String =
    let (r, c) = coords
    columnName c + rowName r

let pieceName (isWhite: bool) (areFigures: bool) (p: Board.Piece): string =
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

let moveName (isWhite: bool) (areFigures: bool) (m: Position.Move): string =
    match (m.Piece, snd m.FromCoords, snd m.ToCoords) with
    | (Board.King, 4, 6) -> "0-0"
    | (Board.King, 4, 2) -> "0-0-0"
    | _ -> let piece = match m.Piece with
                       | Board.Pawn -> ""
                       | _ -> m.Piece
                              |> pieceName isWhite areFigures
                              |> String.map Char.ToUpper
           let clarification =
               match (m.Piece, m.SamePieceCoords) with
               | (Board.Pawn, _) -> m.FromCoords
                                    |> snd
                                    |> columnName
               | (_, Some (_, c)) when c = (snd m.FromCoords) -> m.FromCoords
                                                                 |> fst
                                                                 |> rowName
               | (_, Some _) -> m.FromCoords
                                |> snd
                                |> columnName
               | _ -> ""
           let takes = if m.IsCapture then "x" else ""
           let targetSquare = coordinatesName m.ToCoords
           let promotion = match m.Promotion with
                           | Some p -> pieceName isWhite areFigures p
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
                            (accStr + remaining accNumOpt + pieceName p.IsWhite false p.PieceType, None)
                 )
                 ("", None)
                 row
    lastAccStr + remaining lastAccNumOpt

let positionName (position: Position.Position): string =
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
                    | Some coords -> coordinatesName coords
                    | None -> "-"
    let halfMove = string position.Halfmove
    let fullMove = string position.Fullmove
    sprintf "%s %s %s %s %s %s" board color castling enPassant halfMove fullMove

