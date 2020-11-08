module BlindfoldChessMechanics.NaturalLanguage

open System
open FSharpx.Collections

let words (c: char): string =
    match int c with
    | 75   -> "king"        // 'K'
    | 81   -> "queen"       // 'Q'
    | 82   -> "rook"        // 'R'
    | 66   -> "bishop"      // 'B'
    | 78   -> "knight"      // 'N'
    | 80   -> "pawn"      // 'P'
    | 9812 -> "king"        // '♔'
    | 9813 -> "queen"       // '♕'
    | 9814 -> "rook"        // '♖'
    | 9815 -> "bishop"      // '♗'
    | 9816 -> "knight"      // '♘'
    | 9817 -> "pawn"        // '♙'
    | 9818 -> "king"        // '♚'
    | 9819 -> "queen"       // '♛'
    | 9820 -> "rook"        // '♜'
    | 9821 -> "bishop"      // '♝'
    | 9822 -> "knight"      // '♞'
    | 9823 -> "pawn"        // '♟'
    | 120  -> "takes"       // 'x'
    | 43   -> "check"       // '+'
    | 35   -> "mate"        // '#'
    | 61   -> "promotes to" // '='
    | i    -> (char i).ToString()

let phrase (str: string): string =
    match str with
    | "O-O" -> "castle kingside"
    | "O-O-O" -> "castle queenside"
    | "O-O+" -> "castle kingside check"
    | "O-O-O+" -> "castle queenside check"
    | "O-O#" -> "castle kingside mate"
    | "O-O-O#" -> "castle queenside mate"
    | s -> let ws = s.ToCharArray()
                    |> LazyList.ofArray
                    |> LazyList.filter ((<>) '️')
                    |> LazyList.map words
           String.Join(" ", ws)
