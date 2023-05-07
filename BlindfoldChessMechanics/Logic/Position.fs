module BlindfoldChessMechanics.Logic.Position

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Logic
open FSharpx.Collections

// types

type Movement =
    { Piece: Board.Piece
      FromCoords: Board.Coordinates
      ToCoords: Board.Coordinates
      IsCapture: bool
      Promotion: Board.Piece option }

type Move =
    { Piece: Board.Piece
      FromCoords: Board.Coordinates
      ToCoords: Board.Coordinates
      IsCapture: bool
      Promotion: Board.Piece option
      IsCheck: bool
      IsMate: bool
      IsStalemate: bool
      SamePieceCoords: Board.Coordinates array }

type Castling =
    { WhiteKingSideCastle: bool
      WhiteQueenSideCastle: bool
      BlackKingSideCastle: bool
      BlackQueenSideCastle: bool }

type Position =
    { Board: Board.Board
      IsWhiteToMove: bool
      Castling: Castling
      EnPassant: Board.Coordinates option
      HalfMove: int
      FullMove: int }

// constants

let init =
    { Board = Board.init
      IsWhiteToMove = true
      Castling =
        { WhiteKingSideCastle = true
          WhiteQueenSideCastle = true
          BlackKingSideCastle = true
          BlackQueenSideCastle = true }
      EnPassant = None
      HalfMove = 0
      FullMove = 1 }

// functions

let specialKingMovements (rowIndex, columnIndex as coordinates) position =
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.King
             IsWhite = isWhite } ->
        let controlledCoordsByOpponent =
            Board.coordinatesControlledByColor (not isWhite) position.Board

        [ (true, position.Castling.WhiteKingSideCastle, (+))
          (true, position.Castling.WhiteQueenSideCastle, (-))
          (false, position.Castling.BlackKingSideCastle, (+))
          (false, position.Castling.BlackQueenSideCastle, (-)) ]
        |> LazyList.ofList
        |> LazyList.map (fun (isWh, ability, op) ->
            ability
            && isWh = isWhite
            && Board.resident (rowIndex, op columnIndex 1) position.Board = None
            && Board.resident (rowIndex, op columnIndex 2) position.Board = None
            && (Seq.forall ((<>) coordinates) controlledCoordsByOpponent)
            && (Seq.forall ((<>) (rowIndex, op columnIndex 1)) controlledCoordsByOpponent)
            && (Seq.forall ((<>) (rowIndex, op columnIndex 2)) controlledCoordsByOpponent))
        |> LazyList.zip (
            LazyList.ofList
                [ (rowIndex, columnIndex + 2)
                  (rowIndex, columnIndex - 2)
                  (rowIndex, columnIndex + 2)
                  (rowIndex, columnIndex - 2) ]
        )
        |> LazyList.filter snd
        |> LazyList.map (fun (toCoords, _) ->
            { Piece = Board.King
              FromCoords = coordinates
              ToCoords = toCoords
              IsCapture = false
              Promotion = None })
    | _ -> raise (Board.WrongPiece("NoKing", rowIndex, columnIndex))

let pawnMovements (rowIndex, columnIndex as coordinates) position =
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn } ->
        let enPassantMoveTuples =
            match position.EnPassant with
            | Some(enPassRow, enPassCol) when
                (rowIndex = if position.IsWhiteToMove then
                                enPassRow - 1
                            else
                                enPassRow + 1)
                && (columnIndex = enPassCol + 1 || columnIndex = enPassCol - 1)
                ->
                let toCoords = (enPassRow, enPassCol)
                let isCapture = true
                let promotion = None
                [ (toCoords, isCapture, promotion) ]
            | _ -> []
            |> LazyList.ofList

        let otherMoveTuples =
            Board.coordinatesControlledByPawn coordinates position.Board
            |> LazyList.map (fun toCoords ->
                let toRowIndex, _ = toCoords

                let isCapture =
                    match Board.resident toCoords position.Board with
                    | None -> false
                    | _ -> true

                match toRowIndex with
                | 0
                | 7 ->
                    [ (toCoords, isCapture, Some Board.Queen)
                      (toCoords, isCapture, Some Board.Rook)
                      (toCoords, isCapture, Some Board.Bishop)
                      (toCoords, isCapture, Some Board.Knight) ]
                | _ -> [ (toCoords, isCapture, None) ]
                |> LazyList.ofList)
            |> LazyList.concat

        LazyList.append enPassantMoveTuples otherMoveTuples
        |> LazyList.map (fun (toCoords, isCapture, promotion) ->
            { Piece = Board.Pawn
              FromCoords = coordinates
              ToCoords = toCoords
              IsCapture = isCapture
              Promotion = promotion })
    | _ -> raise (Board.WrongPiece("NoPawn", rowIndex, columnIndex))

let simplePieceMovement fromCoords toCoords position =
    match Board.resident fromCoords position.Board with
    | Some piece ->
        let isCapture =
            match Board.resident toCoords position.Board with
            | None -> false
            | _ -> true

        { Piece = piece.PieceType
          FromCoords = fromCoords
          ToCoords = toCoords
          IsCapture = isCapture
          Promotion = None }
    | _ -> raise (Board.NoPiece fromCoords)

let pieceMovements coordinates position =
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn } -> pawnMovements coordinates position
    | Some { PieceType = Board.King } ->
        Board.coordinatesControlledByKing coordinates position.Board
        |> LazyList.map (fun toCoords -> simplePieceMovement coordinates toCoords position)
        |> LazyList.append (specialKingMovements coordinates position)
    | Some _ ->
        Board.coordinatesControlledByPiece coordinates position.Board
        |> LazyList.map (fun toCoords -> simplePieceMovement coordinates toCoords position)
    | None -> raise (Board.NoPiece coordinates)

let positionAfterMovement (movement: Movement) position =
    let newIsWhiteToMove = not position.IsWhiteToMove

    let newHalfMove =
        if movement.IsCapture || movement.Piece = Board.Pawn then
            0
        else
            position.HalfMove + 1

    let newFullMove =
        if position.IsWhiteToMove then
            position.FullMove
        else
            position.FullMove + 1

    let newEnPassant =
        if
            movement.Piece = Board.Pawn
            && abs (fst movement.FromCoords - fst movement.ToCoords) = 2
        then
            Some((fst movement.FromCoords + fst movement.ToCoords) / 2, snd movement.ToCoords)
        else
            None

    let newWhiteKingSideCastle =
        position.Castling.WhiteKingSideCastle
        && movement.FromCoords <> (0, 4)
        && movement.FromCoords <> (0, 7)

    let newWhiteQueenSideCastle =
        position.Castling.WhiteQueenSideCastle
        && movement.FromCoords <> (0, 4)
        && movement.FromCoords <> (0, 0)

    let newBlackKingSideCastle =
        position.Castling.BlackKingSideCastle
        && movement.FromCoords <> (7, 4)
        && movement.FromCoords <> (7, 7)

    let newBlackQueenSideCastle =
        position.Castling.BlackQueenSideCastle
        && movement.FromCoords <> (7, 4)
        && movement.FromCoords <> (7, 0)

    let enPassUpdatedBoard board =
        match (movement.Piece, position.EnPassant) with
        | Board.Pawn, Some(enPassRow, enPassColumn) when (enPassRow, enPassColumn) = movement.ToCoords ->
            let takenPawnRow =
                if position.IsWhiteToMove then
                    enPassRow - 1
                else
                    enPassRow + 1

            Utils.updatedArrays (takenPawnRow, enPassColumn) None board
        | _ -> board

    let castleUpdatedBoard board =
        let rook =
            Some
                { Board.PieceType = Board.Rook
                  Board.IsWhite = position.IsWhiteToMove }

        match (movement.Piece, snd movement.FromCoords, snd movement.ToCoords) with
        | Board.King, 4, 6 ->
            board
            |> Utils.updatedArrays (fst movement.FromCoords, 7) None
            |> Utils.updatedArrays (fst movement.FromCoords, 5) rook
        | Board.King, 4, 2 ->
            board
            |> Utils.updatedArrays (fst movement.FromCoords, 0) None
            |> Utils.updatedArrays (fst movement.FromCoords, 3) rook
        | _ -> board

    let pieceInNewCoords =
        match movement.Promotion with
        | Some piece ->
            Some
                { Board.PieceType = piece
                  Board.IsWhite = position.IsWhiteToMove }
        | _ ->
            Some
                { Board.PieceType = movement.Piece
                  Board.IsWhite = position.IsWhiteToMove }

    let newBoard =
        position.Board
        |> Utils.updatedArrays movement.FromCoords None
        |> Utils.updatedArrays movement.ToCoords pieceInNewCoords
        |> enPassUpdatedBoard
        |> castleUpdatedBoard

    { Board = newBoard
      IsWhiteToMove = newIsWhiteToMove
      Castling =
        { WhiteKingSideCastle = newWhiteKingSideCastle
          WhiteQueenSideCastle = newWhiteQueenSideCastle
          BlackKingSideCastle = newBlackKingSideCastle
          BlackQueenSideCastle = newBlackQueenSideCastle }
      EnPassant = newEnPassant
      HalfMove = newHalfMove
      FullMove = newFullMove }

let validMovements position =
    Utils.laz2DIndices 8 8
    |> LazyList.filter (fun coords ->
        match Board.resident coords position.Board with
        | Some { Board.IsWhite = isWh } when isWh = position.IsWhiteToMove -> true
        | _ -> false)
    |> LazyList.map (fun coords -> pieceMovements coords position)
    |> LazyList.concat
    |> LazyList.filter (fun move ->
        let newPos = positionAfterMovement move position
        Board.isKingInDanger position.IsWhiteToMove newPos.Board |> not)

let movesWithResultedPosition position =
    let valMovements = validMovements position

    LazyList.map
        (fun move ->
            let newPos = positionAfterMovement move position
            let isCheck = Board.isKingInDanger newPos.IsWhiteToMove newPos.Board
            let canMove = newPos |> validMovements |> LazyList.isEmpty |> not
            let isMate = isCheck && (not canMove)
            let isStalemate = (not isCheck) && (not canMove)

            let samePieceCoords =
                valMovements
                |> LazyList.filter (fun m -> m <> move && m.Piece = move.Piece && m.ToCoords = move.ToCoords)
                |> LazyList.map (fun m -> m.FromCoords)
                |> LazyList.toArray

            let m =
                { Piece = move.Piece
                  FromCoords = move.FromCoords
                  ToCoords = move.ToCoords
                  IsCapture = move.IsCapture
                  Promotion = move.Promotion
                  IsCheck = isCheck
                  IsMate = isMate
                  IsStalemate = isStalemate
                  SamePieceCoords = samePieceCoords }

            (m, newPos))
        valMovements

let positionAfterMove move position =
    let move =
        { Piece = move.Piece
          FromCoords = move.FromCoords
          ToCoords = move.ToCoords
          IsCapture = move.IsCapture
          Promotion = move.Promotion }

    positionAfterMovement move position
