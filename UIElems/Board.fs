module BlindfoldChessTraining.UIElems.Board

open BlindfoldChessTraining
open Fabulous.Maui
open type Fabulous.Maui.View

let imgEmpty () = Image("board/empty.png")

let imgWh () =
    (imgEmpty ()).background Constants.lightSquareColor

let imgBl () =
    (imgEmpty ()).background Constants.darkSquareColor


let img1 () = Image("board/img1")

let img2 () = Image("board/img2")

let img3 () = Image("board/img3")

let img4 () = Image("board/img4")

let img5 () = Image("board/img5")

let img6 () = Image("board/img6")

let img7 () = Image("board/img7")

let img8 () = Image("board/img8")

let imgA () = Image("board/img_a")

let imgB () = Image("board/img_b")

let imgC () = Image("board/img_c")

let imgD () = Image("board/img_d")

let imgE () = Image("board/img_e")

let imgF () = Image("board/img_f")

let imgG () = Image("board/img_g")

let imgH () = Image("board/img_h")

let imgBb () = Image("board/bb")
let imgBk () = Image("board/bk")
let imgBn () = Image("board/bn")
let imgBp () = Image("board/bp")
let imgBq () = Image("board/bq")
let imgBr () = Image("board/br")
let imgWb () = Image("board/wb")
let imgWk () = Image("board/wk")
let imgWn () = Image("board/wn")
let imgWp () = Image("board/wp")
let imgWq () = Image("board/wq")
let imgWr () = Image("board/wr")

//             match resident with
//             | Some { PieceType = Board.King
//                      IsWhite = true } -> [ imgWk ]
//             | Some { PieceType = Board.Queen
//                      IsWhite = true } -> [ imgWq ]
//             | Some { PieceType = Board.Rook
//                      IsWhite = true } -> [ imgWr ]
//             | Some { PieceType = Board.Bishop
//                      IsWhite = true } -> [ imgWb ]
//             | Some { PieceType = Board.Knight
//                      IsWhite = true } -> [ imgWn ]
//             | Some { PieceType = Board.Pawn
//                      IsWhite = true } -> [ imgWp ]
//             | Some { PieceType = Board.King
//                      IsWhite = false } -> [ imgBk ]
//             | Some { PieceType = Board.Queen
//                      IsWhite = false } -> [ imgBq ]
//             | Some { PieceType = Board.Rook
//                      IsWhite = false } -> [ imgBr ]
//             | Some { PieceType = Board.Bishop
//                      IsWhite = false } -> [ imgBb ]
//             | Some { PieceType = Board.Knight
//                      IsWhite = false } -> [ imgBn ]
//             | Some { PieceType = Board.Pawn
//                      IsWhite = false } -> [ imgBp ]

let boardGrid areCoordsEnable boardSizeRatio =
    let maxRowColIndex = if areCoordsEnable then 8 else 7

    let squareWidth =
        boardSizeRatio * Constants.visualWidth / (float maxRowColIndex + 1.)

    let rowColRange = seq { 0..maxRowColIndex }

    (Grid(rowdefs = [ for _ in rowColRange -> Dimension.Auto ], coldefs = [ for _ in rowColRange -> Dimension.Auto ]) {
        for row in rowColRange do
            for col in rowColRange do
                let image =
                    match areCoordsEnable, row, col with
                    | false, r, c when (r + c) % 2 = 0 -> imgWh
                    | false, r, c when (r + c) % 2 = 1 -> imgBl
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
                    | _ -> imgBl

                image().gridRow(row).gridColumn(col).width(squareWidth).height squareWidth
    })
        .centerHorizontal ()

let grid areCoordsEnable boardSizeRatio =
    boardGrid areCoordsEnable boardSizeRatio
