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

type CastlingAvailability =
    { WhiteKingSideCastle: bool
      WhiteQueenSideCastle: bool
      BlackKingSideCastle: bool
      BlackQueenSideCastle: bool }

type EnPassantMove = Board.Coordinates Option

type Position = { CurrentBoard: Board.Board
                  IsWhiteToMove: bool
                  Castling: CastlingAvailability
                  EnPassant: EnPassantMove
                  Halfmove: int
                  Fullmove: int }

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
