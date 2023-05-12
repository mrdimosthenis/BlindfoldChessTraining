module BlindfoldChessTraining.UIElems.Board

open BlindfoldChessMechanics
open BlindfoldChessTraining
open BlindfoldChessTraining.Types
open Fabulous.Maui
open Microsoft.Maui
open type Fabulous.Maui.View
open Microsoft.Maui.Graphics

let imgEmpty = Image("empty.png.png")

let imgWh = Color.FromRgb(216, 208, 246) |> imgEmpty.background

let imgBl = Color.FromRgb(160, 142, 231) |> imgEmpty.background


let img1 = Image("img1.png")

let img2 = Image("img2.png")

let img3 = Image("img3.png")

let img4 = Image("img4.png")

let img5 = Image("img5.png")

let img6 = Image("img6.png")

let img7 = Image("img7.png")

let img8 = Image("img8.png")

let imgA = Image("img_a.png")

let imgB = Image("img_b.png")

let imgC = Image("img_c.png")

let imgD = Image("img_d.png")

let imgE = Image("img_e.png")

let imgF = Image("img_f.png")

let imgG = Image("img_g.png")

let imgH = Image("img_h.png")

let imgBb = Image("bb.png")
let imgBk = Image("bk.png")
let imgBn = Image("bn.png")
let imgBp = Image("bp.png")
let imgBq = Image("bq.png")
let imgBr = Image("br.png")
let imgWb = Image("wb.png")
let imgWk = Image("wk.png")
let imgWn = Image("wn.png")
let imgWp = Image("wp.png")
let imgWq = Image("wq.png")
let imgWr = Image("wr.png")

let residentImg (resident: Logic.Board.Resident) =
    match resident with
    | None -> imgEmpty
    | Some { PieceType = Logic.Board.King
             IsWhite = true } -> imgWk
    | Some { PieceType = Logic.Board.Queen
             IsWhite = true } -> imgWq
    | Some { PieceType = Logic.Board.Rook
             IsWhite = true } -> imgWr
    | Some { PieceType = Logic.Board.Bishop
             IsWhite = true } -> imgWb
    | Some { PieceType = Logic.Board.Knight
             IsWhite = true } -> imgWn
    | Some { PieceType = Logic.Board.Pawn
             IsWhite = true } -> imgWp
    | Some { PieceType = Logic.Board.King
             IsWhite = false } -> imgBk
    | Some { PieceType = Logic.Board.Queen
             IsWhite = false } -> imgBq
    | Some { PieceType = Logic.Board.Rook
             IsWhite = false } -> imgBr
    | Some { PieceType = Logic.Board.Bishop
             IsWhite = false } -> imgBb
    | Some { PieceType = Logic.Board.Knight
             IsWhite = false } -> imgBn
    | Some { PieceType = Logic.Board.Pawn
             IsWhite = false } -> imgBp

let boardGrid areCoordsEnabled boardSizeRatio (board: Logic.Board.Board) =
    let maxRowColIndex = if areCoordsEnabled then 8 else 7

    let squareWidth =
        boardSizeRatio * Constants.visualWidth / (float maxRowColIndex + 1.)

    let rowColRange = seq { 0..maxRowColIndex }

    (Grid(rowdefs = [ for _ in rowColRange -> Dimension.Auto ], coldefs = [ for _ in rowColRange -> Dimension.Auto ]) {
        for row in rowColRange do
            for col in rowColRange do
                let image =
                    match areCoordsEnabled, row, col with
                    | true, 8, 0 -> imgEmpty
                    | true, 8, 1 -> imgA
                    | true, 8, 2 -> imgB
                    | true, 8, 3 -> imgC
                    | true, 8, 4 -> imgD
                    | true, 8, 5 -> imgE
                    | true, 8, 6 -> imgF
                    | true, 8, 7 -> imgG
                    | true, 8, 8 -> imgH
                    | true, 7, 0 -> img1
                    | true, 6, 0 -> img2
                    | true, 5, 0 -> img3
                    | true, 4, 0 -> img4
                    | true, 3, 0 -> img5
                    | true, 2, 0 -> img6
                    | true, 1, 0 -> img7
                    | true, 0, 0 -> img8
                    | true, r, c when (r + c) % 2 = 0 -> imgBl
                    | true, r, c when (r + c) % 2 = 1 -> imgWh
                    | false, r, c when (r + c) % 2 = 0 -> imgWh
                    | false, r, c when (r + c) % 2 = 1 -> imgBl
                    | _ -> imgBl

                image.gridRow(row).gridColumn(col).width(squareWidth).height squareWidth

        for row in rowColRange do
            for col in rowColRange do
                let image =
                    match areCoordsEnabled, row, col with
                    | true, 8, _ -> imgEmpty
                    | true, _, 0 -> imgEmpty
                    | true, r, c -> residentImg (board[7 - r][c - 1])
                    | false, r, c -> residentImg (board[7 - r][c])

                image.gridRow(row).gridColumn(col).width(squareWidth).height squareWidth
    })
        .centerHorizontal()
        .gestureRecognizers () {
        PanGestureRecognizer(fun panArgs ->
            match panArgs.StatusType = GestureStatus.Running, panArgs.TotalX > 0.0 with
            | true, true -> PanRightGesture
            | true, false -> PanLeftGesture
            | _ -> NoOp)
            .touchPoints
            1
    }

let grid model =
    match model.CurrentMoveIndex with
    | None -> model.CurrentGame.InitBoard
    | Some i -> model.CurrentGame.Boards[i]
    |> boardGrid model.ConfigOptions.AreCoordsEnabled model.ConfigOptions.BoardSizeRatio
