module BlindfoldChessMechanics.Model.Board

// types

type Move = int * int * int * int

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type ColoredPiece = {PieceType:Piece; IsWhite:bool}

type Resident = ColoredPiece Option

type Board = Resident seq seq

// constants

let emptySquare: Resident = None

let whiteKing: Resident = Some {PieceType=King; IsWhite=true}
let whiteQueen: Resident = Some {PieceType=Queen; IsWhite=true}
let whiteRook: Resident = Some {PieceType=Rook; IsWhite=true}
let whiteBishop: Resident = Some {PieceType=Bishop; IsWhite=true}
let whiteKnight: Resident = Some {PieceType=Knight; IsWhite=true}
let whitePawn: Resident = Some {PieceType=Pawn; IsWhite=true}

let blackKing: Resident = Some {PieceType=King; IsWhite=false}
let blackQueen: Resident = Some {PieceType=Queen; IsWhite=false}
let blackRook: Resident = Some {PieceType=Rook; IsWhite=false}
let blackBishop: Resident = Some {PieceType=Bishop; IsWhite=false}
let blackKnight: Resident = Some {PieceType=Knight; IsWhite=false}
let blackPawn: Resident = Some {PieceType=Pawn; IsWhite=false}

// utils

let toArrays (board: Board) =
        board
        |> Seq.map Seq.toArray
        |> Seq.toArray

let updatedSeq<'a> (index: int) (newItem: 'a) (s: 'a seq): 'a seq =
    s
    |> Seq.indexed
    |> Seq.map (fun (i, item) ->
                    if i = index then newItem else item
                )

// actual implementation

let init: Board =
        [|[|blackRook; blackKnight; blackBishop; blackQueen; blackKing; blackBishop; blackKnight; blackRook|]
          [|blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn; blackPawn|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare; emptySquare|]
          [|whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn; whitePawn|]
          [|whiteRook; whiteKnight; whiteBishop; whiteQueen; whiteKing; whiteBishop; whiteKnight; whiteRook|]|]
        |> Seq.ofArray
        |> Seq.map Seq.ofArray
        |> Seq.rev

let afterMove (move: Move) (board: Board): Board Option =
        let (fromRowIndex, fromColumnIndex, toRowIndex, toColumnIndex) = move
        let fromRow = Seq.item fromRowIndex board
        let fromResident = Seq.item fromColumnIndex fromRow
        let toRow = Seq.item toRowIndex board
        let toResident = Seq.item toColumnIndex toRow
        match (fromResident, toResident) with
        | (None, _) | (Some {IsWhite=false}, Some {IsWhite=false}) |(Some {IsWhite=true}, Some {IsWhite=true}) -> None
        | _ -> let updatedFromRow = updatedSeq fromColumnIndex None fromRow
               let updatedToRow = updatedSeq toColumnIndex fromResident toRow
               board
               |> updatedSeq fromRowIndex updatedFromRow
               |> updatedSeq toRowIndex updatedToRow
               |> Some
