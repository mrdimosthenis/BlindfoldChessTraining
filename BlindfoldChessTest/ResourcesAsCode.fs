﻿module ResourcesAsCode

let exampleGameJson =
    """{"MetaTags":{"Black":"you","White":"me"},"InitialPosition":{"Board":[[{"Case":"Some","Fields":[{"PieceType":{"Case":"Rook"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Knight"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Bishop"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Queen"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"King"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Bishop"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Knight"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Rook"},"IsWhite":true}]}],[{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},null,{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]}],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":true}]},null,null,null,null],[null,null,null,null,null,null,null,null],[{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},null,{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Pawn"},"IsWhite":false}]}],[{"Case":"Some","Fields":[{"PieceType":{"Case":"Rook"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Knight"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Bishop"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Queen"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"King"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Bishop"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Knight"},"IsWhite":false}]},{"Case":"Some","Fields":[{"PieceType":{"Case":"Rook"},"IsWhite":false}]}]],"IsWhiteToMove":false,"Castling":{"WhiteKingSideCastle":true,"WhiteQueenSideCastle":true,"BlackKingSideCastle":true,"BlackQueenSideCastle":true},"EnPassant":null,"Halfmove":0,"Fullmove":2},"Moves":[{"Piece":{"Case":"Knight"},"FromCoords":{"Item1":7,"Item2":6},"ToCoords":{"Item1":5,"Item2":5},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Bishop"},"FromCoords":{"Item1":0,"Item2":5},"ToCoords":{"Item1":1,"Item2":4},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Pawn"},"FromCoords":{"Item1":6,"Item2":4},"ToCoords":{"Item1":5,"Item2":4},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Pawn"},"FromCoords":{"Item1":1,"Item2":7},"ToCoords":{"Item1":3,"Item2":7},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Bishop"},"FromCoords":{"Item1":7,"Item2":5},"ToCoords":{"Item1":2,"Item2":0},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Pawn"},"FromCoords":{"Item1":1,"Item2":1},"ToCoords":{"Item1":2,"Item2":0},"IsCapture":true,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"King"},"FromCoords":{"Item1":7,"Item2":4},"ToCoords":{"Item1":7,"Item2":6},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"King"},"FromCoords":{"Item1":0,"Item2":4},"ToCoords":{"Item1":0,"Item2":5},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Pawn"},"FromCoords":{"Item1":6,"Item2":2},"ToCoords":{"Item1":4,"Item2":2},"IsCapture":false,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]},{"Piece":{"Case":"Pawn"},"FromCoords":{"Item1":4,"Item2":3},"ToCoords":{"Item1":5,"Item2":2},"IsCapture":true,"Promotion":null,"IsCheck":false,"IsMate":false,"IsStalemate":false,"SamePieceCoords":[]}],"Result":{"Case":"Some","Fields":[{"Case":"Draw"}]}}"""
    