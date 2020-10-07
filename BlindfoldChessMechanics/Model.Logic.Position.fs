module BlindfoldChessMechanics.Model.Logic.Position

open BlindfoldChessMechanics.Model.Logic

exception WrongPiece

// types

type Move = { Piece: Board.Piece
              FromCoords: Board.Coordinates
              ToCoords: Board.Coordinates
              IsCapture: bool
              Promotion: Board.Piece option }

type CastlingAbility =
    { WhiteKingSideCastle: bool
      WhiteQueenSideCastle: bool
      BlackKingSideCastle: bool
      BlackQueenSideCastle: bool }

type Position = { CurrentBoard: Board.Board
                  IsWhiteToMove: bool
                  Castling: CastlingAbility
                  EnPassant: Board.Coordinates Option
                  Halfmove: int
                  Fullmove: int }

let specialKingMoves (coordinates: Board.Coordinates) (position: Position): Move seq =
    let (rowIndex, columnIndex) = coordinates
    match Board.resident coordinates position.CurrentBoard with
    | Some { PieceType = Board.King; IsWhite = isWhite } ->
        let controlledCoordsByOpponent = Board.coordinatesControlledByColor (not isWhite) position.CurrentBoard
        [| (true, position.Castling.WhiteKingSideCastle, (+))
           (true, position.Castling.WhiteQueenSideCastle, (-))
           (false, position.Castling.BlackKingSideCastle, (+))
           (false, position.Castling.BlackQueenSideCastle, (-)) |]
        |> Seq.ofArray
        |> Seq.map ( fun (isWh, ability, op) ->
                        ability && isWh = isWhite
                        && Board.resident (rowIndex, op columnIndex 1) position.CurrentBoard = None
                        && Board.resident (rowIndex, op columnIndex 2) position.CurrentBoard = None
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
    match Board.resident coordinates position.CurrentBoard with
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
        let otherMoveTuples = Board.coordinatesControlledByPawn coordinates position.CurrentBoard
                              |> Seq.map (fun toCoords ->
                                            let (toRowIndex, _) = toCoords
                                            let isCapture =
                                                match Board.resident toCoords position.CurrentBoard with
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
