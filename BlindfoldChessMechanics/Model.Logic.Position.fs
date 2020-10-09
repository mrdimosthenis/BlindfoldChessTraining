module BlindfoldChessMechanics.Model.Logic.Position

open BlindfoldChessMechanics.Model.Logic
open BlindfoldChessMechanics

exception WrongPiece

// types

type Move = { Piece: Board.Piece
              FromCoords: Board.Coordinates
              ToCoords: Board.Coordinates
              IsCapture: bool
              Promotion: Board.Piece option }

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

let specialKingMoves (coordinates: Board.Coordinates) (position: Position): Move seq =
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
    | _ -> raise WrongPiece

let pawnMoves (coordinates: Board.Coordinates) (position: Position): Move seq =
    let (rowIndex, columnIndex) = coordinates
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn; IsWhite = isWhite } ->
        let enPassantMoveTuples =
            match position.EnPassant with
            | Some (enPasRow, enPasCol) when (rowIndex = enPasRow) && ( columnIndex = enPasCol + 1 || columnIndex = enPasCol - 1 ) ->
                let toRowIndex = if isWhite then rowIndex + 1 else rowIndex - 1
                let toCoords = (toRowIndex, enPasCol)
                let isCapture = true
                let promotion = None
                [|(toCoords, isCapture, promotion)|]
            | _ -> [||]
            |> Seq.ofArray
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
        Seq.append enPassantMoveTuples otherMoveTuples
        |> Seq.map (fun (toCoords, isCapture, promotion) ->
                        { Piece = Board.Pawn
                          FromCoords = coordinates
                          ToCoords = toCoords
                          IsCapture = isCapture
                          Promotion = promotion }
                   )
    | _ -> raise WrongPiece

let simplePieceMove (fromCoords: Board.Coordinates) (toCoords: Board.Coordinates) (position: Position): Move =
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
    | _ -> raise Board.NoPieceInBoard

let pieceMoves (coordinates: Board.Coordinates) (position: Position): Move seq =
    match Board.resident coordinates position.Board with
    | Some { PieceType = Board.Pawn} ->
        pawnMoves coordinates position
    | Some { PieceType = Board.King} ->
        Board.coordinatesControlledByKing coordinates position.Board
        |> Seq.map (fun toCoords -> simplePieceMove coordinates toCoords position)
        |> Seq.append (specialKingMoves coordinates position)
    | Some piece ->
        Board.coordinatesControlledByPiece coordinates position.Board
        |> Seq.map (fun toCoords -> simplePieceMove coordinates toCoords position)
    | None ->
        raise Board.NoPieceInBoard

let positionAfterMove (move: Move) (position: Position): Position =
    let newIsWhiteToMove = not position.IsWhiteToMove
    let newHalfMove = if move.IsCapture || move.Piece = Board.Pawn then 0
                      else position.Halfmove + 1
    let newFullMove = if position.IsWhiteToMove then position.Fullmove
                      else position.Fullmove + 1
    let newEnPassant =
        if move.Piece = Board.Pawn && abs(fst move.FromCoords - fst move.ToCoords) = 2 then Some move.ToCoords
        else None
    let newWhiteKingSideCastle = position.Castling.WhiteKingSideCastle && move.FromCoords <> (0, 4) && move.FromCoords <> (0, 7)
    let newWhiteQueenSideCastle = position.Castling.WhiteQueenSideCastle && move.FromCoords <> (0, 4) && move.FromCoords <> (0, 0)
    let newBlackKingSideCastle = position.Castling.BlackKingSideCastle && move.FromCoords <> (7, 4) && move.FromCoords <> (7, 7)
    let newBlackQueenSideCastle = position.Castling.BlackQueenSideCastle && move.FromCoords <> (7, 4) && move.FromCoords <> (7, 0)
    let pieceInNewCoords =
        match move.Promotion with
        | Some piece -> Some { Board.PieceType = piece; Board.IsWhite = position.IsWhiteToMove }
        | _ -> Some { Board.PieceType = move.Piece; Board.IsWhite = position.IsWhiteToMove }
    let newBoard = position.Board
                   |> Utils.updatedSequences move.FromCoords None
                   |> Utils.updatedSequences move.ToCoords pieceInNewCoords
    { Board = newBoard
      IsWhiteToMove = newIsWhiteToMove
      Castling = { WhiteKingSideCastle = newWhiteKingSideCastle
                   WhiteQueenSideCastle = newWhiteQueenSideCastle
                   BlackKingSideCastle = newBlackKingSideCastle
                   BlackQueenSideCastle = newBlackQueenSideCastle } 
      EnPassant = newEnPassant
      Halfmove = newHalfMove
      Fullmove = newFullMove }

// implementation

//let possibleMovesWithResultedPosition
//    (coordinates: Board.Coordinates)
//    (position: Position)
//    : (Move * Position) seq =
//    match Board.resident coordinates position.CurrentBoard with
//    | Some { PieceType = piece; IsWhite = isWhite } ->
//        position.CurrentBoard
//        |> Board.indicesControlledByPiece coordinates
//        |> Seq.map (fun toCoords ->
//                        let isCapture =
//                            match Board.resident toCoords position.CurrentBoard with
//                            | Some _ -> true
//                            | _ -> false
//                        let promotion = 
//
//                        let nextCurrentBoard =
//                            
//                        let isCheck =
//                            Board.isKingInDanger
//                   )
//    | _ -> raise Board.NoPieceInBoard
