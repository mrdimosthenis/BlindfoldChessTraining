﻿module BlindfoldChessMechanics.Model.Board

exception InvalidMove
exception NoPieceInPosition

// types

type Move = int * int * int * int

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type ColoredPiece = {PieceType:Piece; IsWhite:bool}

type Resident = ColoredPiece Option

type Board = Resident seq seq

// constants

let emptySquare: Resident = None

let whiteKing: Resident = Some {PieceType=King; IsWhite=true}
let whiteQueen: Resident = Some {PieceType=Queen; IsWhite=true}
let whiteRook: Resident = Some {PieceType=Rook; IsWhite=true}
let whiteBishop: Resident = Some {PieceType=Bishop; IsWhite=true}
let whiteKnight: Resident = Some {PieceType=Knight; IsWhite=true}
let whitePawn: Resident = Some {PieceType=Pawn; IsWhite=true}

let blackKing: Resident = Some {PieceType=King; IsWhite=false}
let blackQueen: Resident = Some {PieceType=Queen; IsWhite=false}
let blackRook: Resident = Some {PieceType=Rook; IsWhite=false}
let blackBishop: Resident = Some {PieceType=Bishop; IsWhite=false}
let blackKnight: Resident = Some {PieceType=Knight; IsWhite=false}
let blackPawn: Resident = Some {PieceType=Pawn; IsWhite=false}

// utils

let toArrays (board: Board): Resident array array =
    board
    |> Seq.map Seq.toArray
    |> Seq.toArray

let ofArrays (arr: Resident array array): Board =
    arr
    |> Seq.ofArray
    |> Seq.map Seq.ofArray

let updatedSeq<'a> (index: int) (newItem: 'a) (s: 'a seq): 'a seq =
    s
    |> Seq.indexed
    |> Seq.map (fun (i, item) ->
                    if i = index then newItem else item
                )

// actual implementation

let init: Board =
    [|[|blackRook; blackKnight; blackBishop; blackQueen; blackKing; blackBishop; blackKnight; blackRook|]
      [|blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn|]
      [|whiteRook; whiteKnight; whiteBishop; whiteQueen; whiteKing; whiteBishop; whiteKnight; whiteRook|]|]
    |> Seq.ofArray
    |> Seq.map Seq.ofArray
    |> Seq.rev
        
let afterMove (move: Move) (board: Board): Board =
    let (fromRowIndex, fromColumnIndex, toRowIndex, toColumnIndex) = move
    let fromRow = Seq.item fromRowIndex board
    let fromResident = Seq.item fromColumnIndex fromRow
    let toRow = Seq.item toRowIndex board
    let toResident = Seq.item toColumnIndex toRow
    match (fromResident, toResident) with
    | (None, _) | (Some {IsWhite=false}, Some {IsWhite=false}) |(Some {IsWhite=true}, Some {IsWhite=true}) -> raise InvalidMove
    | _ -> let updatedFromRow = updatedSeq fromColumnIndex None fromRow
           let updatedToRow = updatedSeq toColumnIndex fromResident toRow
           board
           |> updatedSeq fromRowIndex updatedFromRow
           |> updatedSeq toRowIndex updatedToRow

let upIndices (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    Seq.init (7 - rowIndex) (fun i -> (rowIndex + i + 1, columnIndex))

let downIndices (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    Seq.init rowIndex (fun i -> (rowIndex - i - 1, columnIndex))

let leftIndices (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    Seq.init columnIndex (fun i -> (rowIndex, columnIndex - i - 1))

let rightIndices (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    Seq.init (7 - columnIndex) (fun i -> (rowIndex, columnIndex + i + 1))

let upRightDiagonal (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    let minDist = min (7 - rowIndex) (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex + i + 1))

let downRightDiagonal (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    let minDist = min rowIndex (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex + i + 1))

let upLeftDiagonal (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    let minDist = min (7 - rowIndex) columnIndex
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex - i - 1))

let downLeftDiagonal (position: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = position
    let minDist = min rowIndex columnIndex
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex - i - 1))

let resident (position: int * int) (board: Board): Resident =
    let (rowIndex, columnIndex) = position
    board
    |> Seq.item rowIndex
    |> Seq.item columnIndex

let indicesControlledByRook (position: int * int) (board: Board): (int * int) seq =
    match resident position board with
    | None ->
        raise NoPieceInPosition
    | Some {IsWhite=isWhite}
        -> let takeOrNot pos = match resident pos board with
                               | Some {IsWhite=w} when w = isWhite -> false
                               | _ -> true
           [|position |> upIndices |> Seq.takeWhile takeOrNot
             position |> downIndices |> Seq.takeWhile takeOrNot
             position |> rightIndices |> Seq.takeWhile takeOrNot
             position |> leftIndices |> Seq.takeWhile takeOrNot|]
           |> Seq.ofArray
           |> Seq.concat

let indicesControlledByBishop (position: int * int) (board: Board): (int * int) seq =
    match resident position board with
    | None ->
        raise NoPieceInPosition
    | Some {IsWhite=isWhite}
        -> let takeOrNot pos = match resident pos board with
                               | Some {IsWhite=w} when w = isWhite -> false
                               | _ -> true
           [|position |> upRightDiagonal |> Seq.takeWhile takeOrNot
             position |> downRightDiagonal |> Seq.takeWhile takeOrNot
             position |> upLeftDiagonal |> Seq.takeWhile takeOrNot
             position |> downLeftDiagonal |> Seq.takeWhile takeOrNot|]
           |> Seq.ofArray
           |> Seq.concat

let indicesControlledByQueen (position: int * int) (board: Board): (int * int) seq =
    [|indicesControlledByRook position board
      indicesControlledByBishop position board|]
    |> Seq.ofArray
    |> Seq.concat
