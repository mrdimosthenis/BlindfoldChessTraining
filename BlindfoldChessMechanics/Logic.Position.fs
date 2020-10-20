module BlindfoldChessMechanics.Logic.Position

open BlindfoldChessMechanics.Logic
open BlindfoldChessMechanics
open System

// types

type Movement = { Piece: Board.Piece
                  FromCoords: Board.Coordinates
                  ToCoords: Board.Coordinates
                  IsCapture: bool
                  Promotion: Board.Piece option }

type Move = { Piece: Board.Piece
              FromCoords: Board.Coordinates
              ToCoords: Board.Coordinates
              IsCapture: bool
              Promotion: Board.Piece option
              IsCheck: bool
              IsMate: bool
              IsStalemate: bool
              SamePieceCoords: Board.Coordinates option }

type Castling =
    { WhiteKingSideCastle: bool
      WhiteQueenSideCastle: bool
      BlackKingSideCastle: bool
      BlackQueenSideCastle: bool }

type Position = { Board: Board.Board
                  IsWhiteToMove: bool
                  Castling: Castling
                  EnPassant: Board.Coordinates Option
                  Halfmove: int
                  Fullmove: int }

// constants

let init: Position = { Board = Board.init
                       IsWhiteToMove = true
                       Castling = { WhiteKingSideCastle = true
                                    WhiteQueenSideCastle = true
                                    BlackKingSideCastle = true
                                    BlackQueenSideCastle = true } 
                       EnPassant = None
                       Halfmove = 0
                       Fullmove = 1 }

// functions

let specialKingMovements (coordinates: Board.Coordinates) (position: Position): Movement seq =
    let (rowIndex, columnIndex) = coordinates
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.King; IsWhite = isWhite } ->
        let controlledCoordsByOpponent = Board.coordinatesControlledByColor (not isWhite) position.Board
        [| (true, position.Castling.WhiteKingSideCastle, (+))
           (true, position.Castling.WhiteQueenSideCastle, (-))
           (false, position.Castling.BlackKingSideCastle, (+))
           (false, position.Castling.BlackQueenSideCastle, (-)) |]
        |> Seq.ofArray
        |> Seq.map ( fun (isWh, ability, op) ->
                        ability && isWh = isWhite
                        && Board.resident (rowIndex, op columnIndex 1) position.Board = None
                        && Board.resident (rowIndex, op columnIndex 2) position.Board = None
                        && (Seq.forall ((<>) coordinates) controlledCoordsByOpponent)
                        && (Seq.forall ((<>) (rowIndex, op columnIndex 1)) controlledCoordsByOpponent)
                        && (Seq.forall ((<>) (rowIndex, op columnIndex 2)) controlledCoordsByOpponent)
                   )
        |> Seq.zip (Seq.ofArray [| (rowIndex, columnIndex + 2)
                                   (rowIndex, columnIndex - 2)
                                   (rowIndex, columnIndex + 2)
                                   (rowIndex, columnIndex - 2) |])
        |> Seq.filter snd
        |> Seq.map (fun (toCoords, _) ->
                        { Piece = Board.King
                          FromCoords = coordinates
                          ToCoords = toCoords
                          IsCapture = false
                          Promotion = None }
                   )
        |> Seq.cache
    | _ -> raise (Board.WrongPiece ("NoKing", rowIndex, columnIndex))

let pawnMovements (coordinates: Board.Coordinates) (position: Position): Movement seq =
    let (rowIndex, columnIndex) = coordinates
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn; IsWhite = isWhite } ->
        let enPassantMoveTuples =
            match position.EnPassant with
            | Some (enPassRow, enPassCol) when (rowIndex = if position.IsWhiteToMove then enPassRow - 1 else enPassRow + 1) &&
                                                    ( columnIndex = enPassCol + 1 || columnIndex = enPassCol - 1 ) ->
                let toCoords = (enPassRow, enPassCol)
                let isCapture = true
                let promotion = None
                [|(toCoords, isCapture, promotion)|]
            | _ -> [||]
            |> Seq.ofArray
            |> Seq.cache
        let otherMoveTuples = Board.coordinatesControlledByPawn coordinates position.Board
                              |> Seq.map (fun toCoords ->
                                            let (toRowIndex, _) = toCoords
                                            let isCapture =
                                                match Board.resident toCoords position.Board with
                                                | None -> false
                                                | _ -> true
                                            match toRowIndex with
                                            | 0 | 7 ->
                                                [| (toCoords, isCapture, Some Board.Queen)
                                                   (toCoords, isCapture, Some Board.Rook)
                                                   (toCoords, isCapture, Some Board.Bishop)
                                                   (toCoords, isCapture, Some Board.Knight) |]
                                            | _ ->
                                                [|(toCoords, isCapture, None)|]
                                            |> Seq.ofArray
                                         )
                              |> Seq.concat
                              |> Seq.cache
        Seq.append enPassantMoveTuples otherMoveTuples
        |> Seq.map (fun (toCoords, isCapture, promotion) ->
                        { Piece = Board.Pawn
                          FromCoords = coordinates
                          ToCoords = toCoords
                          IsCapture = isCapture
                          Promotion = promotion }
                   )
        |> Seq.cache
    | _ -> raise (Board.WrongPiece ("NoPawn", rowIndex, columnIndex))

let simplePieceMovement (fromCoords: Board.Coordinates) (toCoords: Board.Coordinates) (position: Position): Movement =
    match Board.resident fromCoords position.Board with
    | Some piece -> let isCapture =
                        match Board.resident toCoords position.Board with
                        | None -> false
                        | _ -> true
                    { Piece = piece.PieceType
                      FromCoords = fromCoords
                      ToCoords = toCoords
                      IsCapture = isCapture
                      Promotion = None }
    | _ -> raise (Board.NoPiece fromCoords)

let pieceMovements (coordinates: Board.Coordinates) (position: Position): Movement seq =
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn} ->
        pawnMovements coordinates position
    | Some { PieceType = Board.King} ->
        Board.coordinatesControlledByKing coordinates position.Board
        |> Seq.map (fun toCoords -> simplePieceMovement coordinates toCoords position)
        |> Seq.append (specialKingMovements coordinates position)
    | Some piece ->
        Board.coordinatesControlledByPiece coordinates position.Board
        |> Seq.map (fun toCoords -> simplePieceMovement coordinates toCoords position)
    | None ->
        raise (Board.NoPiece coordinates)
    |> Seq.cache

let positionAfterMovement (movement: Movement) (position: Position): Position =
    let newIsWhiteToMove = not position.IsWhiteToMove
    let newHalfMove = if movement.IsCapture || movement.Piece = Board.Pawn then 0
                      else position.Halfmove + 1
    let newFullMove = if position.IsWhiteToMove then position.Fullmove
                      else position.Fullmove + 1
    let newEnPassant =
        if movement.Piece = Board.Pawn && abs(fst movement.FromCoords - fst movement.ToCoords) = 2
            then Some ((fst movement.FromCoords + fst movement.ToCoords)/2, snd movement.ToCoords)
        else
            None
    let newWhiteKingSideCastle = position.Castling.WhiteKingSideCastle && movement.FromCoords <> (0, 4) && movement.FromCoords <> (0, 7)
    let newWhiteQueenSideCastle = position.Castling.WhiteQueenSideCastle && movement.FromCoords <> (0, 4) && movement.FromCoords <> (0, 0)
    let newBlackKingSideCastle = position.Castling.BlackKingSideCastle && movement.FromCoords <> (7, 4) && movement.FromCoords <> (7, 7)
    let newBlackQueenSideCastle = position.Castling.BlackQueenSideCastle && movement.FromCoords <> (7, 4) && movement.FromCoords <> (7, 0)
    let enPassUpdatedBoard (board: Board.Board): Board.Board =
        match (movement.Piece, position.EnPassant) with
        | (Board.Pawn, Some (enPassRow, enPassColumn)) when (enPassRow, enPassColumn) = movement.ToCoords ->
            let takenPawnRow = if position.IsWhiteToMove then enPassRow - 1
                               else enPassRow + 1
            Utils.updatedArrays (takenPawnRow, enPassColumn) None board
        | _ ->
            board
    let castleUpdatedBoard (board: Board.Board): Board.Board =
        let rook = Some {Board.PieceType = Board.Rook; Board.IsWhite = position.IsWhiteToMove}
        match (movement.Piece, snd movement.FromCoords, snd movement.ToCoords) with
        | (Board.King, 4, 6) -> board
                                |> Utils.updatedArrays (fst movement.FromCoords, 7) None
                                |> Utils.updatedArrays (fst movement.FromCoords, 5) rook
        | (Board.King, 4, 2) -> board
                                |> Utils.updatedArrays (fst movement.FromCoords, 0) None
                                |> Utils.updatedArrays (fst movement.FromCoords, 3) rook
        | _ -> board
    let pieceInNewCoords =
        match movement.Promotion with
        | Some piece -> Some { Board.PieceType = piece; Board.IsWhite = position.IsWhiteToMove }
        | _ -> Some { Board.PieceType = movement.Piece; Board.IsWhite = position.IsWhiteToMove }
    let newBoard = position.Board
                   |> Utils.updatedArrays movement.FromCoords None
                   |> Utils.updatedArrays movement.ToCoords pieceInNewCoords
                   |> enPassUpdatedBoard
                   |> castleUpdatedBoard
    { Board = newBoard
      IsWhiteToMove = newIsWhiteToMove
      Castling = { WhiteKingSideCastle = newWhiteKingSideCastle
                   WhiteQueenSideCastle = newWhiteQueenSideCastle
                   BlackKingSideCastle = newBlackKingSideCastle
                   BlackQueenSideCastle = newBlackQueenSideCastle } 
      EnPassant = newEnPassant
      Halfmove = newHalfMove
      Fullmove = newFullMove }

let validMovements (position: Position): Movement seq =
    seq {
        for rowIndex in 0 .. 7 do
            for columnIndex in 0 .. 7 ->
                (rowIndex, columnIndex)
    }
    |> Seq.filter (fun coords ->
                        match Board.resident coords position.Board with
                        | Some {Board.IsWhite = isWh} when isWh = position.IsWhiteToMove -> true
                        | _ -> false
                  )
    |> Seq.map (fun coords -> pieceMovements coords position)
    |> Seq.concat
    |> Seq.filter (fun move ->
                        let newPos = positionAfterMovement move position
                        Board.isKingInDanger position.IsWhiteToMove newPos.Board
                        |> not
                  )
    |> Seq.cache

let movesWithResultedPosition (position: Position): (Move * Position) seq =
    let valMovements = validMovements position
    valMovements
    |> Seq.map (fun move ->
                    let newPos = positionAfterMovement move position
                    let isCheck =
                        Board.isKingInDanger newPos.IsWhiteToMove newPos.Board
                    let canMove = newPos
                                  |> validMovements
                                  |> Seq.isEmpty
                                  |> not
                    let isMate = isCheck && (not canMove)
                    let isStalemate = (not isCheck) && (not canMove)
                    let samePieceCoords =
                        valMovements
                        |> Seq.filter (fun m -> m <> move && m.Piece = move.Piece && m.ToCoords = move.ToCoords)
                        |> Seq.map (fun m -> m.FromCoords)
                        |> Seq.tryHead
                    let m = 
                        { Piece = move.Piece
                          FromCoords = move.FromCoords
                          ToCoords = move.ToCoords
                          IsCapture = move.IsCapture
                          Promotion = move.Promotion
                          IsCheck = isCheck
                          IsMate = isMate
                          IsStalemate = isStalemate
                          SamePieceCoords= samePieceCoords }
                    (m, newPos)
                )
    |> Seq.cache

let positionAfterMove (move: Move) (position: Position): Position =
    let move = { Piece = move.Piece
                 FromCoords = move.FromCoords
                 ToCoords = move.ToCoords
                 IsCapture = move.IsCapture
                 Promotion = move.Promotion }
    positionAfterMovement move position
