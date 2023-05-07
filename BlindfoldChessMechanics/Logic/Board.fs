module BlindfoldChessMechanics.Logic.Board

open BlindfoldChessMechanics
open FSharpx.Collections

exception NoPiece of int * int
exception WrongPiece of string * int * int

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

type Board = Resident array array

// constants

let emptySquare = None

let whiteKing =
    Some { PieceType = King; IsWhite = true }

let whiteQueen =
    Some { PieceType = Queen; IsWhite = true }

let whiteRook =
    Some { PieceType = Rook; IsWhite = true }

let whiteBishop =
    Some { PieceType = Bishop; IsWhite = true }

let whiteKnight =
    Some { PieceType = Knight; IsWhite = true }

let whitePawn =
    Some { PieceType = Pawn; IsWhite = true }

let blackKing =
    Some { PieceType = King; IsWhite = false }

let blackQueen =
    Some { PieceType = Queen; IsWhite = false }

let blackRook =
    Some { PieceType = Rook; IsWhite = false }

let blackBishop =
    Some { PieceType = Bishop; IsWhite = false }

let blackKnight =
    Some { PieceType = Knight; IsWhite = false }

let blackPawn =
    Some { PieceType = Pawn; IsWhite = false }

let empty: Board =
    [|[|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]|]
    |> Array.rev

let init =
    [|[|blackRook;   blackKnight; blackBishop; blackQueen;  blackKing;   blackBishop; blackKnight; blackRook|]
      [|blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn;   blackPawn|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
      [|whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn;   whitePawn|]
      [|whiteRook;   whiteKnight; whiteBishop; whiteQueen;  whiteKing;   whiteBishop; whiteKnight; whiteRook|]|]
    |> Array.rev

// functions

let areValidCoordinates (rowIndex, columnIndex) =
    rowIndex >= 0 && rowIndex <= 7 && columnIndex >= 0 && columnIndex <= 7

let upCoordinates (rowIndex, columnIndex) =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take (7 - rowIndex)
    |> LazyList.map (fun i -> (rowIndex + i + 1, columnIndex))

let downCoordinates (rowIndex, columnIndex) =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take rowIndex
    |> LazyList.map (fun i -> (rowIndex - i - 1, columnIndex))

let leftCoordinates (rowIndex, columnIndex) =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take columnIndex
    |> LazyList.map (fun i -> (rowIndex, columnIndex - i - 1))

let rightCoordinates (rowIndex, columnIndex) =
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take (7 - columnIndex)
    |> LazyList.map (fun i -> (rowIndex, columnIndex + i + 1))

let upRightDiagonal (rowIndex, columnIndex) =
    let minDist = min (7 - rowIndex) (7 - columnIndex)
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take minDist
    |> LazyList.map (fun i -> (rowIndex + i + 1, columnIndex + i + 1))

let downRightDiagonal (rowIndex, columnIndex) =
    let minDist = min rowIndex (7 - columnIndex)
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take minDist
    |> LazyList.map (fun i -> (rowIndex - i - 1, columnIndex + i + 1))

let upLeftDiagonal (rowIndex, columnIndex) =
    let minDist = min (7 - rowIndex) columnIndex
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take minDist
    |> LazyList.map (fun i -> (rowIndex + i + 1, columnIndex - i - 1))

let downLeftDiagonal (rowIndex, columnIndex) =
    let minDist = min rowIndex columnIndex
    id
    |> Seq.initInfinite
    |> LazyList.ofSeq
    |> LazyList.take minDist
    |> LazyList.map (fun i -> (rowIndex - i - 1, columnIndex - i - 1))

let resident (rowIndex, columnIndex) (board: Board) =
    board[rowIndex].[columnIndex]

let collectControlledCoordinates directionF coordinates board =
    match resident coordinates board with
    | None -> raise (NoPiece coordinates)
    | Some piece ->
        coordinates
        |> directionF
        |> LazyList.fold
            (fun (accCoords, accMetPiece) coord ->
                        if accMetPiece then
                            (accCoords, accMetPiece)
                        else
                            match resident coord board with
                            | Some { IsWhite = w } when w = piece.IsWhite -> (accCoords, true)
                            | Some _ -> (Utils.prependedLaz coord accCoords, true)
                            | _ -> (Utils.prependedLaz coord accCoords, false)
                            )
            (LazyList.empty, false)
        |> fst

let coordinatesControlledByRook coordinates board =
    [ upCoordinates
      downCoordinates
      rightCoordinates
      leftCoordinates ]
    |> LazyList.ofList
    |> LazyList.map (fun dirF -> collectControlledCoordinates dirF coordinates board)
    |> LazyList.concat

let coordinatesControlledByBishop coordinates board =
    [ upRightDiagonal
      downRightDiagonal
      upLeftDiagonal
      downLeftDiagonal ]
    |> LazyList.ofList
    |> LazyList.map (fun dirF -> collectControlledCoordinates dirF coordinates board)
    |> LazyList.concat

let coordinatesControlledByQueen coordinates board =
    [ coordinatesControlledByRook coordinates board
      coordinatesControlledByBishop coordinates board ]
    |> LazyList.ofList
    |> LazyList.concat

let coordinatesControlledByKnight (rowIndex, columnIndex) board =
    match resident (rowIndex, columnIndex) board with
    | Some { PieceType = Knight
             IsWhite = isWhite } ->
        [ (rowIndex - 2, columnIndex - 1)
          (rowIndex - 1, columnIndex - 2)
          (rowIndex + 2, columnIndex - 1)
          (rowIndex + 1, columnIndex - 2)
          (rowIndex - 2, columnIndex + 1)
          (rowIndex - 1, columnIndex + 2)
          (rowIndex + 2, columnIndex + 1)
          (rowIndex + 1, columnIndex + 2) ]
        |> LazyList.ofList
        |> LazyList.filter areValidCoordinates
        |> LazyList.filter
            (fun coord ->
                    match resident coord board with
                    | Some { IsWhite = w } when w = isWhite -> false
                    | _ -> true
            )
    | _ -> raise (WrongPiece ("NoKnight", rowIndex, columnIndex))

let coordinatesControlledByKing (rowIndex, columnIndex) board =
    match resident (rowIndex, columnIndex) board with
    | Some { PieceType = King; IsWhite = isWhite } ->
        [ (rowIndex - 1, columnIndex - 1)
          (rowIndex - 1, columnIndex)
          (rowIndex - 1, columnIndex + 1)
          (rowIndex, columnIndex - 1)
          (rowIndex, columnIndex + 1)
          (rowIndex + 1, columnIndex - 1)
          (rowIndex + 1, columnIndex)
          (rowIndex + 1, columnIndex + 1) ]
        |> LazyList.ofList
        |> LazyList.filter areValidCoordinates
        |> LazyList.filter
                (fun coord ->
                        match resident coord board with
                        | Some { IsWhite = w } when w = isWhite -> false
                        | _ -> true
                )
    | _ -> raise (WrongPiece ("NoKing", rowIndex, columnIndex))

let coordinatesControlledByPawn (rowIndex, columnIndex) board =
    match resident (rowIndex, columnIndex) board with
    | Some { PieceType = Pawn; IsWhite = isWhite } ->
        let rowIncrease = if isWhite then 1 else -1
        let nextRowIndex = rowIndex + rowIncrease
        let forwardCoords = (nextRowIndex, columnIndex)

        let twoSquaresForward =
            match (isWhite, rowIndex, resident forwardCoords board) with
            | true, 1, None
            | false, 6, None -> [ ((nextRowIndex + rowIncrease, columnIndex), false) ]
            | _ -> []
            |> LazyList.ofList

        let diagCoordsWithCapt =
            [ ((nextRowIndex, columnIndex - 1), true)
              ((nextRowIndex, columnIndex + 1), true) ]
            |> LazyList.ofList
            |> LazyList.filter (fun (coords, _) -> areValidCoordinates coords)

        LazyList.append twoSquaresForward diagCoordsWithCapt
        |> Utils.prependedLaz (forwardCoords, false)
        |> LazyList.filter
                (fun (coords, isCapt) ->
                        match (resident coords board, isCapt) with
                        | Some piece, true -> piece.IsWhite <> isWhite
                        | None, false -> true
                        | _ -> false
                )
        |> LazyList.map fst
    | _ -> raise (WrongPiece ("NoPawn", rowIndex, columnIndex))

let coordinatesControlledByPiece coordinates board =
    let coordsF =
        match resident coordinates board with
        | Some { PieceType = Rook } -> coordinatesControlledByRook
        | Some { PieceType = Bishop } -> coordinatesControlledByBishop
        | Some { PieceType = Queen } -> coordinatesControlledByQueen
        | Some { PieceType = Knight } -> coordinatesControlledByKnight
        | Some { PieceType = King } -> coordinatesControlledByKing
        | Some { PieceType = Pawn } -> coordinatesControlledByPawn
        | _ -> raise (NoPiece coordinates)

    coordsF coordinates board

let coordinatesControlledByColor isWhite board =
    Utils.laz2DIndices 8 8
    |> LazyList.filter (fun coords ->
        match resident coords board with
        | Some { IsWhite = isWh } when isWh = isWhite -> true
        | _ -> false)
    |> LazyList.map (fun coords -> coordinatesControlledByPiece coords board)
    |> LazyList.concat
    |> Seq.distinct
    |> LazyList.ofSeq

let isKingInDanger isWhite board =
    board
    |> coordinatesControlledByColor (not isWhite)
    |> Seq.exists
            (fun coords ->
                    match resident coords board with
                    | Some { PieceType = King; IsWhite = isWh } when isWh = isWhite -> true
                    | _ -> false
            )
            