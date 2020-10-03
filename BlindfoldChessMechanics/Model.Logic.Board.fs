module BlindfoldChessMechanics.Model.Logic.Board

exception NoPieceInBoard

// types

type Piece =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn

type ColoredPiece = { PieceType: Piece; IsWhite: bool }

type Resident = ColoredPiece Option

type Board = Resident seq seq

// constants

let emptySquare: Resident = None

let whiteKing: Resident =
    Some { PieceType = King; IsWhite = true }

let whiteQueen: Resident =
    Some { PieceType = Queen; IsWhite = true }

let whiteRook: Resident =
    Some { PieceType = Rook; IsWhite = true }

let whiteBishop: Resident =
    Some { PieceType = Bishop; IsWhite = true }

let whiteKnight: Resident =
    Some { PieceType = Knight; IsWhite = true }

let whitePawn: Resident =
    Some { PieceType = Pawn; IsWhite = true }

let blackKing: Resident =
    Some { PieceType = King; IsWhite = false }

let blackQueen: Resident =
    Some { PieceType = Queen; IsWhite = false }

let blackRook: Resident =
    Some { PieceType = Rook; IsWhite = false }

let blackBishop: Resident =
    Some { PieceType = Bishop; IsWhite = false }

let blackKnight: Resident =
    Some { PieceType = Knight; IsWhite = false }

let blackPawn: Resident =
    Some { PieceType = Pawn; IsWhite = false }

// utils

let toArrays<'a> (s: 'a seq seq): 'a array array = s |> Seq.map Seq.toArray |> Seq.toArray

let ofArrays<'a> (arr: 'a array array): 'a seq seq =
    arr |> Seq.ofArray |> Seq.map Seq.ofArray

let updatedSeq<'a> (index: int) (newItem: 'a) (s: 'a seq): 'a seq =
    s
    |> Seq.indexed
    |> Seq.map (fun (i, item) -> if i = index then newItem else item)

let prependedSeq<'a> (a: 'a) (s: 'a seq): 'a seq = Seq.append (seq [ a ]) s

// actual implementation

let init: Board =
    [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
      [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
      [|whiteRook;   whiteKnight; whiteBishop; whiteQueen; whiteKing;    whiteBishop; whiteKnight; whiteRook|]|]
    |> Seq.ofArray
    |> Seq.map Seq.ofArray
    |> Seq.rev

let areValidCoordinates(coordinates: int * int): bool =
    let (rowIndex, columnIndex) = coordinates

    rowIndex >= 0 && rowIndex <= 7 && columnIndex >= 0 && columnIndex <= 7

let upIndices (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init (7 - rowIndex) (fun i -> (rowIndex + i + 1, columnIndex))

let downIndices (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init rowIndex (fun i -> (rowIndex - i - 1, columnIndex))

let leftIndices (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init columnIndex (fun i -> (rowIndex, columnIndex - i - 1))

let rightIndices (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init (7 - columnIndex) (fun i -> (rowIndex, columnIndex + i + 1))

let upRightDiagonal (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min (7 - rowIndex) (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex + i + 1))

let downRightDiagonal (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min rowIndex (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex + i + 1))

let upLeftDiagonal (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min (7 - rowIndex) columnIndex
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex - i - 1))

let downLeftDiagonal (coordinates: int * int): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min rowIndex columnIndex
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex - i - 1))

let resident (coordinates: int * int) (board: Board): Resident =
    let (rowIndex, columnIndex) = coordinates
    board |> Seq.item rowIndex |> Seq.item columnIndex

let collectControlledIndices (coordinates: int * int)
                             (board: Board)
                             (directionF: int * int -> (int * int) seq)
                             : (int * int) seq =
    match resident coordinates board with
    | None -> raise NoPieceInBoard
    | Some piece ->
        coordinates
        |> directionF
        |> Seq.fold (fun (accIndices, accMetPiece) coord ->
            if accMetPiece then
                (accIndices, accMetPiece)
            else
                match resident coord board with
                | Some { IsWhite = w } when w = piece.IsWhite -> (accIndices, true)
                | Some _ -> (prependedSeq coord accIndices, true)
                | _ -> (prependedSeq coord accIndices, false)) (Seq.empty, false)
        |> fst

let indicesControlledByRook (coordinates: int * int) (board: Board): (int * int) seq =
    [| upIndices
       downIndices
       rightIndices
       leftIndices |]
    |> Seq.ofArray
    |> Seq.map (collectControlledIndices coordinates board)
    |> Seq.concat

let indicesControlledByBishop (coordinates: int * int) (board: Board): (int * int) seq =
    [| upRightDiagonal
       downRightDiagonal
       upLeftDiagonal
       downLeftDiagonal |]
    |> Seq.ofArray
    |> Seq.map (collectControlledIndices coordinates board)
    |> Seq.concat

let indicesControlledByQueen (coordinates: int * int) (board: Board): (int * int) seq =
    [| indicesControlledByRook coordinates board
       indicesControlledByBishop coordinates board |]
    |> Seq.ofArray
    |> Seq.concat

let indicesControlledByKnight (coordinates: int * int) (board: Board): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates

    match resident coordinates board with
    | Some { PieceType = Knight
             IsWhite = isWhite } ->
        [| (rowIndex - 2, columnIndex - 1)
           (rowIndex - 1, columnIndex - 2)
           (rowIndex + 2, columnIndex - 1)
           (rowIndex + 1, columnIndex - 2)
           (rowIndex - 2, columnIndex + 1)
           (rowIndex - 1, columnIndex + 2)
           (rowIndex + 2, columnIndex + 1)
           (rowIndex + 1, columnIndex + 2) |]
        |> Seq.ofArray
        |> Seq.filter areValidCoordinates
        |> Seq.filter (fun coord ->
            match resident coord board with
            | Some { IsWhite = w } when w = isWhite -> false
            | _ -> true)
    | _ -> raise NoPieceInBoard

let indicesControlledByKing (coordinates: int * int) (board: Board): (int * int) seq =
    let (rowIndex, columnIndex) = coordinates

    match resident coordinates board with
    | Some { PieceType = King
             IsWhite = isWhite } ->
        [| (rowIndex - 1, columnIndex - 1)
           (rowIndex - 1, columnIndex)
           (rowIndex - 1, columnIndex + 1)
           (rowIndex, columnIndex - 1)
           (rowIndex, columnIndex + 1)
           (rowIndex + 1, columnIndex - 1)
           (rowIndex + 1, columnIndex)
           (rowIndex + 1, columnIndex + 1) |]
        |> Seq.ofArray
        |> Seq.filter areValidCoordinates
        |> Seq.filter (fun coord ->
            match resident coord board with
            | Some { IsWhite = w } when w = isWhite -> false
            | _ -> true)
    | _ -> raise NoPieceInBoard
