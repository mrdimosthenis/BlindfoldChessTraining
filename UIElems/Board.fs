module BlindfoldChessTraining.UIElems.Board

open BlindfoldChessMechanics
open BlindfoldChessTraining.Types
open Fabulous.Maui
open Microsoft.Maui
open Microsoft.Maui.Devices
open type Fabulous.Maui.View
open Microsoft.Maui.Graphics

let imgEmpty = Image("board/empty.png")

let imgWh = Color.FromRgb(216, 208, 246) |> imgEmpty.background

let imgBl = Color.FromRgb(160, 142, 231) |> imgEmpty.background


let img1 = Image("board/img1")

let img2 = Image("board/img2")

let img3 = Image("board/img3")

let img4 = Image("board/img4")

let img5 = Image("board/img5")

let img6 = Image("board/img6")

let img7 = Image("board/img7")

let img8 = Image("board/img8")

let imgA = Image("board/img_a")

let imgB = Image("board/img_b")

let imgC = Image("board/img_c")

let imgD = Image("board/img_d")

let imgE = Image("board/img_e")

let imgF = Image("board/img_f")

let imgG = Image("board/img_g")

let imgH = Image("board/img_h")

let imgBb = Image("board/bb")
let imgBk = Image("board/bk")
let imgBn = Image("board/bn")
let imgBp = Image("board/bp")
let imgBq = Image("board/bq")
let imgBr = Image("board/br")
let imgWb = Image("board/wb")
let imgWk = Image("board/wk")
let imgWn = Image("board/wn")
let imgWp = Image("board/wp")
let imgWq = Image("board/wq")
let imgWr = Image("board/wr")

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
    
let visualWidth =
    DeviceDisplay.MainDisplayInfo.Width / DeviceDisplay.MainDisplayInfo.Density

let boardGrid areCoordsEnabled boardSizeRatio (board: Logic.Board.Board) =
    let maxRowColIndex = if areCoordsEnabled then 8 else 7

    let squareWidth = boardSizeRatio * visualWidth / (float maxRowColIndex + 1.)

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
