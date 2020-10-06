module BlindfoldChessMechanics.Model.Logic.Position

open BlindfoldChessMechanics.Model.Logic

exception InvalidMove

// types

type PromotedPiece =
    | Queen
    | Rook
    | Bishop
    | Knight

type Move = { Piece: Board.Piece
              FromCoords: Board.Coordinates
              ToCoords: Board.Coordinates
              IsCapture: bool
              IsCheck: bool
              Promotion: PromotedPiece Option }

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

type CastleMove =
    | WhiteKingSideCastle 
    | WhiteQueenSideCastle
    | BlackKingSideCastle
    | BlackQueenSideCastle

let specialKingMoves (coordinates: Board.Coordinates) (position: Position): Move seq =
    let (rowIndex, columnIndex) = coordinates
    match Board.resident coordinates position.CurrentBoard with
    | Some { PieceType = Board.King; IsWhite = isWhite } ->
        let controlledCoordsByOpponent = Board.coordinatesControlledByColor (not isWhite) position.CurrentBoard
        [| (WhiteKingSideCastle, position.Castling.WhiteKingSideCastle, (+))
           (WhiteQueenSideCastle, position.Castling.WhiteQueenSideCastle, (-))
           (BlackKingSideCastle, position.Castling.BlackKingSideCastle, (+))
           (BlackQueenSideCastle, position.Castling.BlackQueenSideCastle, (-)) |]
        |> Seq.ofArray
        |> Seq.map ( fun (castleMove, ability, op) ->
                        (
                            castleMove,
                            ability && (Seq.forall ((<>) coordinates) controlledCoordsByOpponent)
                                    && (Seq.forall ((<>) (op rowIndex 1, columnIndex)) controlledCoordsByOpponent)
                                    && (Seq.forall ((<>) (op rowIndex 2, columnIndex)) controlledCoordsByOpponent)
                        )
                   )
        |> Seq.filter snd
        |> Seq.map (fun (castleMove, _) ->
                        match castleMove with
                        | WhiteKingSideCastle | BlackKingSideCastle -> (rowIndex + 2, columnIndex)
                        | WhiteQueenSideCastle | BlackQueenSideCastle -> (rowIndex - 2, columnIndex)
                   )
        |> Seq.map (fun toCoords ->
                        { Piece = Board.King
                          FromCoords = coordinates
                          ToCoords = toCoords
                          IsCapture = false
                          IsCheck = false
                          Promotion= None }
                   )
    | _ -> raise Board.NoPieceInBoard

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
