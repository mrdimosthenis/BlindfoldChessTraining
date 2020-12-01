module BlindfoldChessEngine.Fitting.Encode

open FSharpx.Collections

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics.Notation

// types

type PositionalEvaluation =
          | Centipawns of float
          | WhiteMate of int
          | BlackMate of int

// helper functions

let boolFloat (b: bool): float =
    if b then 1.0 else 0.0

let parsedEvaluation(str: string): PositionalEvaluation =
    match str with
    | "1-0" ->
        WhiteMate 0
    | "0-1" ->
        BlackMate 0
    | s when s.Contains("#-") ->
        s.Substring(2)
        |> int
        |> BlackMate
    | s when s.Contains("#")
        -> s.Substring(1)
        |> int
        |> WhiteMate
    | s ->
        s
        |> float
        |> Centipawns

// encoders

let residentFloats (resident: Board.Resident): float LazyList =
    match resident with
    | None ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.King; Board.IsWhite = true } ->
        [ 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Queen; Board.IsWhite = true } ->
        [ 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Rook; Board.IsWhite = true } ->
        [ 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Bishop; Board.IsWhite = true } ->
        [ 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Knight; Board.IsWhite = true } ->
        [ 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Pawn; Board.IsWhite = true } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.King; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Queen; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Rook; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Bishop; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0 ]
    | Some { Board.PieceType = Board.Knight; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0 ]
    | Some { Board.PieceType = Board.Pawn; Board.IsWhite = false } ->
        [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 1.0 ]
    |> LazyList.ofList

let boardFloats (board: Board.Board): float LazyList =
    board
    |> BlindfoldChessMechanics.Utils.lazOfArrays
    |> LazyList.concat
    |> LazyList.map residentFloats
    |> LazyList.concat

let isWhiteToMoveFloats (isWhiteToMove: bool): float LazyList =
    [ boolFloat isWhiteToMove ]
    |> LazyList.ofList

let castlingFloats (castling: Position.Castling): float LazyList =
    [ castling.WhiteKingSideCastle
      castling.WhiteQueenSideCastle
      castling.BlackKingSideCastle
      castling.BlackQueenSideCastle ]
    |> LazyList.ofList
    |> LazyList.map boolFloat

let enPassantFloats (enPassant: Board.Coordinates option): float LazyList =
    match enPassant with
    | None ->
        LazyList.ofList [ 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0 ]
    | Some (_, c) ->
        LazyList.repeat 0.0
        |> LazyList.take 8
        |> BlindfoldChessMechanics.Utils.updatedLaz c 1.0

let halfmoveFloats (halfmove: int): float LazyList =
    [ (min (float halfmove) 100.0) / 100.0 ]
    |> LazyList.ofList

let fenFloats (fen: string): float LazyList =
    let pos = Parser.textOfFen fen
    [ boardFloats pos.Board
      isWhiteToMoveFloats pos.IsWhiteToMove
      castlingFloats pos.Castling
      enPassantFloats pos.EnPassant
      halfmoveFloats pos.Halfmove ]
    |> LazyList.ofList
    |> LazyList.concat

let scale (maxAbs: float) (num: float): float =
    num
    |> min maxAbs
    |> max (0.0 - maxAbs)
    |> (*) (1.0 / (2.0 * maxAbs))
    |> (+) 0.5

let microEvaluationFloat (ev: PositionalEvaluation): float =
    match ev with
    | WhiteMate i ->
        1.0
    | BlackMate i ->
        0.0
    | Centipawns num ->
        scale 5.0 num

let macroEvaluationFloat (ev: PositionalEvaluation): float =
    match ev with
    | WhiteMate i ->
        1.0
    | BlackMate i ->
        0.0
    | Centipawns num ->
        scale 154.14 num

let mateEvaluationFloat (ev: PositionalEvaluation): float =
    match ev with
    | WhiteMate i ->
        71 - i
        |> max 0
        |> float
        |> scale 71.0
    | BlackMate i ->
        i - 71
        |> min 0
        |> float
        |> scale 71.0
    | Centipawns _ ->
        0.5

let evaluationFloats (s: string): float LazyList =
    let parsedEval = parsedEvaluation s
    [ microEvaluationFloat parsedEval
      macroEvaluationFloat parsedEval
      mateEvaluationFloat parsedEval ]
    |> LazyList.ofList
