module BlindfoldChessMechanics.Notation.Emit

open BlindfoldChessMechanics.Logic

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

let pieceName (isWhite: bool) (areFigures: bool) (p: Board.Piece): string =
    match (p, areFigures, isWhite) with
    | (Board.King, false, _) -> "K"
    | (Board.Queen, false, _) -> "Q"
    | (Board.Rook, false, _) -> "R"
    | (Board.Bishop, false, _) -> "B"
    | (Board.Knight, false, _) -> "N"
    | (Board.Pawn, false, _) -> "P"
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

let movementName (isWhite: bool) (areFigures: bool) (m: Position.Movement): string =
    match (m.Piece, snd m.FromCoords, snd m.ToCoords) with
    | (Board.King, 4, 6) -> "0-0"
    | (Board.King, 4, 2) -> "0-0-0"
    | _ -> let piece = match m.Piece with
                       | Board.Pawn -> ""
                       | _ -> pieceName isWhite areFigures m.Piece
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
           let targetColumn = m.ToCoords
                              |> snd
                              |> columnName
           let targetSquare = m.ToCoords
                              |> fst
                              |> rowName
           let promotion = match m.Promotion with
                           | Some p -> "=" + pieceName isWhite areFigures p
                           | None -> ""
           let checkOrMate = match (m.IsCheck, m.IsMate) with
                             | (_, true) -> "#"
                             | (true, _) -> "+"
                             | _ -> ""
           piece + clarification + takes + targetColumn + targetSquare + promotion + checkOrMate


