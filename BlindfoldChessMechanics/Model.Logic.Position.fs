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
