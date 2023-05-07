module BlindfoldChessMechanics.Notation.Emitter

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Logic
open FSharpx.Collections
open System
open System.IO

exception InvalidColumn of string

// functions

let rowText r = string (r + 1)

let columnText c =
    match c with
    | 0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | 4 -> "e"
    | 5 -> "f"
    | 6 -> "g"
    | 7 -> "h"
    | _ -> c |> string |> InvalidColumn |> raise

let coordinatesText (r, c) = columnText c + rowText r

let pieceText isWhite areFigures p =
    match (p, areFigures, isWhite) with
    | Board.King, false, true -> "K"
    | Board.Queen, false, true -> "Q"
    | Board.Rook, false, true -> "R"
    | Board.Bishop, false, true -> "B"
    | Board.Knight, false, true -> "N"
    | Board.Pawn, false, true -> "P"
    | Board.King, false, false -> "k"
    | Board.Queen, false, false -> "q"
    | Board.Rook, false, false -> "r"
    | Board.Bishop, false, false -> "b"
    | Board.Knight, false, false -> "n"
    | Board.Pawn, false, false -> "p"
    | Board.King, true, true -> "♔"
    | Board.Queen, true, true -> "♕"
    | Board.Rook, true, true -> "♖"
    | Board.Bishop, true, true -> "♗"
    | Board.Knight, true, true -> "♘"
    | Board.Pawn, true, true -> "♙"
    | Board.King, true, false -> "♚"
    | Board.Queen, true, false -> "♛"
    | Board.Rook, true, false -> "♜"
    | Board.Bishop, true, false -> "♝"
    | Board.Knight, true, false -> "♞"
    | Board.Pawn, true, false -> "♟︎"

let textsOfPieces areFigures board =
    let piecesWithColorAndPriorityGroups =
        Utils.laz2DIndices 8 8
        |> LazyList.map (fun (i, j) ->
            match Board.resident (i, j) board with
            | Some piece ->
                let pieceDescription =
                    piece.PieceType |> pieceText piece.IsWhite areFigures |> String.map Char.ToUpper

                let description = pieceDescription + columnText j + rowText i

                let priority =
                    match piece.PieceType with
                    | Board.King -> 1
                    | Board.Pawn -> 2
                    | Board.Knight -> 3
                    | Board.Bishop -> 4
                    | Board.Rook -> 5
                    | Board.Queen -> 6

                [ (description, piece.IsWhite, priority) ]
            | None -> []
            |> LazyList.ofList)
        |> LazyList.concat
        |> Seq.sortBy (fun (_, _, p) -> p)
        |> Seq.groupBy (fun (_, w, _) -> w)
        |> LazyList.ofSeq
        |> LazyList.map (fun (k, s) -> (k, LazyList.ofSeq s))

    let whitePieces =
        piecesWithColorAndPriorityGroups
        |> LazyList.find fst
        |> snd
        |> LazyList.map (fun (d, _, _) -> d)

    let blackPieces =
        piecesWithColorAndPriorityGroups
        |> LazyList.find (fun t -> t |> fst |> not)
        |> snd
        |> LazyList.map (fun (d, _, _) -> d)

    (whitePieces, blackPieces)

let moveText isWhite areFigures (m: Position.Move) =
    let fromRow, fromColumn = m.FromCoords

    match (m.Piece, fromColumn, snd m.ToCoords, m.IsCheck, m.IsMate) with
    | Board.King, 4, 6, false, false -> "O-O"
    | Board.King, 4, 2, false, false -> "O-O-O"
    | Board.King, 4, 6, true, false -> "O-O+"
    | Board.King, 4, 2, true, false -> "O-O-O+"
    | Board.King, 4, 6, true, true -> "O-O#"
    | Board.King, 4, 2, true, true -> "O-O-O#"
    | _ ->
        let piece =
            match m.Piece with
            | Board.Pawn -> ""
            | _ -> m.Piece |> pieceText isWhite areFigures |> String.map Char.ToUpper

        let fromRowText, fromColumnText = rowText fromRow, columnText fromColumn

        let clarification =
            match (m.Piece, m.SamePieceCoords, m.IsCapture) with
            | Board.Pawn, _, true -> fromColumnText
            | Board.Pawn, _, false
            | _, [||], _ -> ""
            | _, samePieceCoords, _ ->
                let isSameColumn = Array.exists (fun (_, c) -> c = fromColumn) samePieceCoords
                let isSameRow = Array.exists (fun (r, _) -> r = fromRow) samePieceCoords

                match (isSameColumn, isSameRow) with
                | true, true -> fromColumnText + fromRowText
                | true, false -> fromRowText
                | _ -> fromColumnText

        let takes = if m.IsCapture then "x" else ""
        let targetSquare = coordinatesText m.ToCoords

        let promotion =
            match m.Promotion with
            | Some p -> pieceText isWhite areFigures p |> String.map Char.ToUpper |> (+) "="
            | None -> ""

        let checkOrMate =
            match (m.IsCheck, m.IsMate) with
            | _, true -> "#"
            | true, _ -> "+"
            | _ -> ""

        piece + clarification + takes + targetSquare + promotion + checkOrMate

let moveTextsWithNumberIndicators areFigures isWhiteToMove moves =
    let indices =
        id
        |> Seq.initInfinite
        |> LazyList.ofSeq
        |> LazyList.map (fun i -> [ i + 1; i + 1 ])
        |> LazyList.map LazyList.ofList
        |> LazyList.concat
        |> (if isWhiteToMove then id else LazyList.tail)

    let isWhiteToMoveBools =
        id
        |> Seq.initInfinite
        |> LazyList.ofSeq
        |> LazyList.map (fun _ -> if isWhiteToMove then [ true; false ] else [ false; true ])
        |> LazyList.map LazyList.ofList
        |> LazyList.concat

    LazyList.zip indices isWhiteToMoveBools
    |> LazyList.zip moves
    |> LazyList.map (fun (m, (i, b)) ->
        let numberIndicators =
            match (b, i, isWhiteToMove) with
            | true, _, _ -> [ ($"{i}.", true) ]
            | false, 1, false -> [ ($"{i}...", true) ]
            | false, _, _ -> []
            |> LazyList.ofList

        [ (moveText b areFigures m, false) ]
        |> LazyList.ofList
        |> LazyList.append numberIndicators)
    |> LazyList.concat

let multipleMovesText areFigures isWhiteToMove moves =
    let textsWithNumbers = moveTextsWithNumberIndicators areFigures isWhiteToMove moves
    let head, tail = LazyList.uncons textsWithNumbers

    LazyList.zip tail textsWithNumbers
    |> LazyList.map (fun ((s, _), (_, prevIndicator)) -> (s, prevIndicator))
    |> Utils.prependedLaz head
    |> LazyList.fold (fun acc (s, prevIndicator) -> if prevIndicator then acc + s else acc + " " + s) ""

let rowFEN row =
    let remaining (opt: int option) : string =
        match opt with
        | Some i -> string i
        | None -> ""

    let lastAccStr, lastAccNumOpt =
        Array.fold
            (fun (accStr, accNumOpt) resident ->
                match (resident, accNumOpt) with
                | None, None -> (accStr, Some 1)
                | None, Some i -> (accStr, Some(i + 1))
                | Some p: Board.Resident, _ ->
                    (accStr + remaining accNumOpt + pieceText p.IsWhite false p.PieceType, None))
            ("", None)
            row

    lastAccStr + remaining lastAccNumOpt

let positionText (position: Position.Position) =
    let board = position.Board |> Array.rev |> Array.map rowFEN |> String.concat "/"
    let color = if position.IsWhiteToMove then "w" else "b"

    let castling =
        [ (position.Castling.WhiteKingSideCastle, "K")
          (position.Castling.WhiteQueenSideCastle, "Q")
          (position.Castling.BlackKingSideCastle, "k")
          (position.Castling.BlackQueenSideCastle, "q") ]
        |> LazyList.ofList
        |> LazyList.filter fst
        |> LazyList.map snd
        |> String.concat ""
        |> (fun s -> if s = "" then "-" else s)

    let enPassant =
        match position.EnPassant with
        | Some coords -> coordinatesText coords
        | None -> "-"

    let halfMove = string position.HalfMove
    let fullMove = string position.FullMove
    $"{board} {color} {castling} {enPassant} {halfMove} {fullMove}"

let metaTagsText (fen, metaTags) =
    let fenKVs =
        if fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" then
            LazyList.empty
        else
            LazyList.ofList [ ("FEN", fen) ]

    let lines =
        metaTags
        |> Map.toSeq
        |> Seq.append fenKVs
        |> Seq.map (fun (k, v) -> $"""[{k} "{v}"]""")
        |> Seq.cache

    String.Join("\n", lines)

let gameText (game: Game.Game) =
    let fen = positionText game.InitialPosition
    let metaTags = metaTagsText (fen, game.MetaTags)

    let moves =
        game.Moves
        |> LazyList.ofArray
        |> multipleMovesText false game.InitialPosition.IsWhiteToMove

    let result =
        match game.Result with
        | None -> "*"
        | Some Game.White -> "1-0"
        | Some Game.Black -> "0-1"
        | Some Game.Draw -> "1/2-1/2"

    $"{metaTags}\n\n{moves}  {result}"

let gameFileTexts filePath games =
    let w = File.AppendText filePath

    LazyList.iter
        (fun g ->
            w.WriteLine(gameText g)
            w.WriteLine())
        games

    w.Close()

let gameJson game =
    Newtonsoft.Json.JsonConvert.SerializeObject(game)

let gameFileJsons filePath games =
    let w = File.AppendText filePath
    LazyList.iter (fun g -> w.WriteLine(gameJson g)) games
    w.Close()
