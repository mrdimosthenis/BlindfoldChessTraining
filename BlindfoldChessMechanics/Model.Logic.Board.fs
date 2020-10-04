module BlindfoldChessMechanics.Model.Logic.Board

open BlindfoldChessMechanics

exception NoPieceInBoard

// types

type Coordinates = int * int

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

// implementation

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

let areValidCoordinates(coordinates: Coordinates): bool =
    let (rowIndex, columnIndex) = coordinates

    rowIndex >= 0 && rowIndex <= 7 && columnIndex >= 0 && columnIndex <= 7

let upIndices (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init (7 - rowIndex) (fun i -> (rowIndex + i + 1, columnIndex))

let downIndices (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init rowIndex (fun i -> (rowIndex - i - 1, columnIndex))

let leftIndices (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init columnIndex (fun i -> (rowIndex, columnIndex - i - 1))

let rightIndices (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    Seq.init (7 - columnIndex) (fun i -> (rowIndex, columnIndex + i + 1))

let upRightDiagonal (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min (7 - rowIndex) (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex + i + 1))

let downRightDiagonal (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min rowIndex (7 - columnIndex)
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex + i + 1))

let upLeftDiagonal (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min (7 - rowIndex) columnIndex
    Seq.init minDist (fun i -> (rowIndex + i + 1, columnIndex - i - 1))

let downLeftDiagonal (coordinates: Coordinates): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates
    let minDist = min rowIndex columnIndex
    Seq.init minDist (fun i -> (rowIndex - i - 1, columnIndex - i - 1))

let resident (coordinates: Coordinates) (board: Board): Resident =
    let (rowIndex, columnIndex) = coordinates
    board |> Seq.item rowIndex |> Seq.item columnIndex

let collectControlledIndices (directionF: Coordinates -> Coordinates seq)
                             (coordinates: Coordinates)
                             (board: Board)
                             : Coordinates seq =
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
                | Some _ -> (Utils.prependedSeq coord accIndices, true)
                | _ -> (Utils.prependedSeq coord accIndices, false)) (Seq.empty, false)
        |> fst

let indicesControlledByRook (coordinates: Coordinates) (board: Board): Coordinates seq =
    [| upIndices
       downIndices
       rightIndices
       leftIndices |]
    |> Seq.ofArray
    |> Seq.map (fun dirF -> collectControlledIndices dirF coordinates board)
    |> Seq.concat

let indicesControlledByBishop (coordinates: Coordinates) (board: Board): Coordinates seq =
    [| upRightDiagonal
       downRightDiagonal
       upLeftDiagonal
       downLeftDiagonal |]
    |> Seq.ofArray
    |> Seq.map (fun dirF -> collectControlledIndices dirF coordinates board)
    |> Seq.concat

let indicesControlledByQueen (coordinates: Coordinates) (board: Board): Coordinates seq =
    [| indicesControlledByRook coordinates board
       indicesControlledByBishop coordinates board |]
    |> Seq.ofArray
    |> Seq.concat

let indicesControlledByKnight (coordinates: Coordinates) (board: Board): Coordinates seq =
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

let indicesControlledByKing (coordinates: Coordinates) (board: Board): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates

    match resident coordinates board with
    | Some { PieceType = King; IsWhite = isWhite } ->
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

let indicesControlledByPawn (coordinates: Coordinates) (board: Board): Coordinates seq =
    let (rowIndex, columnIndex) = coordinates

    match resident coordinates board with
    | Some { PieceType = Pawn; IsWhite = isWhite } ->
        let rowIncrease = if isWhite then 1 else -1
        let nextRowIndex = rowIndex + rowIncrease
        let forwardCoords = (nextRowIndex, columnIndex)

        let twoSquaresForward =
            match (isWhite, rowIndex, resident forwardCoords board) with
            | (true, 1, None)
            | (false, 6, None) -> [| ((nextRowIndex + rowIncrease, columnIndex), false) |]
            | _ -> [||]
            |> Seq.ofArray

        let diagCoordsWithCapt =
            [| ((nextRowIndex, columnIndex - 1), true)
               ((nextRowIndex, columnIndex + 1), true) |]
            |> Seq.ofArray
            |> Seq.filter (fun (coords, _) -> areValidCoordinates coords)

        Seq.append twoSquaresForward diagCoordsWithCapt
        |> Utils.prependedSeq (forwardCoords, false)
        |> Seq.filter (fun (coords, isCapt) ->
            match (resident coords board, isCapt) with
            | (Some piece, true) -> piece.IsWhite <> isWhite
            | (None, false) -> true
            | _ -> false)
        |> Seq.map fst
    | _ -> raise NoPieceInBoard

let indicesControlledByPiece (coordinates: Coordinates) (board: Board): Coordinates seq =
    let indicesF =
        match resident coordinates board with
        | Some { PieceType = Rook } -> indicesControlledByRook
        | Some { PieceType = Bishop } -> indicesControlledByBishop
        | Some { PieceType = Queen } -> indicesControlledByQueen
        | Some { PieceType = Knight } -> indicesControlledByKnight
        | Some { PieceType = King } -> indicesControlledByKing
        | Some { PieceType = Pawn } -> indicesControlledByPawn
        | _ -> raise NoPieceInBoard

    indicesF coordinates board

let indicesControlledByColor (isWhite: bool) (board: Board): Coordinates seq =
    seq {
        for rowIndex in 0 .. 7 do
            for columnIndex in 0 .. 7 ->
                (rowIndex, columnIndex)
    }
    |> Seq.filter (fun coords ->
        match resident coords board with
        | Some { IsWhite = isWh } when isWh = isWhite -> true
        | _ -> false)
    |> Seq.map (fun coords -> indicesControlledByPiece coords board)
    |> Seq.concat
    |> Seq.distinct

let isKingInDanger (isWhite: bool) (board: Board): bool =
    board
    |> indicesControlledByColor (not isWhite)
    |> Seq.exists (fun coords ->
                    match resident coords board with
                    | Some { PieceType = King; IsWhite = isWh } when isWh = isWhite -> true
                    | _ -> false
                  )