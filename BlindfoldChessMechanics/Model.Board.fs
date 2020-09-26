module Model.Board

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type ColoredPiece = {PieceType:Piece; IsWhite:bool}

type Resident = ColoredPiece Option

type Row = Resident array

type Board = Row array

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

let init: Board =
            Array.rev
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
