﻿module BlindfoldChessTraining.UIElems.Board

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Logic
open FSharpx.Collections
open BlindfoldChessTraining

exception InvalidRow of int

let image (name: string): ViewElement =
    let pngImgSrc =
        name |> sprintf "board.%s" |> Resources.image

    View.Image(source = pngImgSrc)


let imgWh: ViewElement =
    View.Image(backgroundColor = Constants.lightSquareColor)

let imgBl: ViewElement =
    View.Image(backgroundColor = Constants.darkSquareColor)

let imgEmpty = image "empty"

let img1: ViewElement = image "1"
let img2: ViewElement = image "2"
let img3: ViewElement = image "3"
let img4: ViewElement = image "4"
let img5: ViewElement = image "5"
let img6: ViewElement = image "6"
let img7: ViewElement = image "7"
let img8: ViewElement = image "8"
let imgA: ViewElement = image "a"
let imgB: ViewElement = image "b"
let imgC: ViewElement = image "c"
let imgD: ViewElement = image "d"
let imgE: ViewElement = image "e"
let imgF: ViewElement = image "f"
let imgG: ViewElement = image "g"
let imgH: ViewElement = image "h"
let imgBb: ViewElement = image "bb"
let imgBk: ViewElement = image "bk"
let imgBn: ViewElement = image "bn"
let imgBp: ViewElement = image "bp"
let imgBq: ViewElement = image "bq"
let imgBr: ViewElement = image "br"
let imgWb: ViewElement = image "wb"
let imgWk: ViewElement = image "wk"
let imgWn: ViewElement = image "wn"
let imgWp: ViewElement = image "wp"
let imgWq: ViewElement = image "wq"
let imgWr: ViewElement = image "wr"

let bottomRow: ViewElement LazyList =
    [ imgEmpty
      imgA
      imgB
      imgC
      imgD
      imgE
      imgF
      imgG
      imgH ]
    |> LazyList.ofList
    |> Seq.indexed
    |> LazyList.ofSeq
    |> LazyList.map (fun (i, v) -> v.Row(8).Column(i))

let emptyRow (areCoordsEnabled: bool) (r: int): ViewElement LazyList =
    let innerRowElems =
        if r % 2 = 0 then [ imgBl; imgWh ] else [ imgWh; imgBl ]
        |> LazyList.ofList
        |> LazyList.repeat
        |> LazyList.take 4
        |> LazyList.concat

    if areCoordsEnabled then
        let fstElem =
            match r with
            | 0 -> img1
            | 1 -> img2
            | 2 -> img3
            | 3 -> img4
            | 4 -> img5
            | 5 -> img6
            | 6 -> img7
            | 7 -> img8
            | _ -> raise (InvalidRow r)

        innerRowElems
        |> Utils.prependedLaz fstElem
        |> Seq.indexed
        |> LazyList.ofSeq
        |> LazyList.map (fun (i, v) -> v.Row(7 - r).Column(i))
    else
        innerRowElems
        |> Seq.indexed
        |> LazyList.ofSeq
        |> LazyList.map (fun (i, v) -> v.Row(7 - r).Column(i))

let emptyBoard (areCoordsEnabled: bool): ViewElement LazyList =
    let middleRowElems =
        id
        |> Seq.initInfinite
        |> LazyList.ofSeq
        |> LazyList.take 8
        |> LazyList.map (emptyRow areCoordsEnabled)
        |> LazyList.concat

    if areCoordsEnabled then [ middleRowElems; bottomRow ] else [ middleRowElems ]
    |> LazyList.ofList
    |> LazyList.concat

let pieces (areCoordsEnabled: bool) (board: Board.Board): ViewElement LazyList =
    board
    |> Utils.lazOfArrays
    |> Seq.indexed
    |> LazyList.ofSeq
    |> LazyList.map (fun (rowIndex, row) ->
        row
        |> Seq.indexed
        |> LazyList.ofSeq
        |> LazyList.map (fun (columnIndex, resident) ->
            match resident with
            | Some { PieceType = Board.King
                     IsWhite = true } -> [ imgWk ]
            | Some { PieceType = Board.Queen
                     IsWhite = true } -> [ imgWq ]
            | Some { PieceType = Board.Rook
                     IsWhite = true } -> [ imgWr ]
            | Some { PieceType = Board.Bishop
                     IsWhite = true } -> [ imgWb ]
            | Some { PieceType = Board.Knight
                     IsWhite = true } -> [ imgWn ]
            | Some { PieceType = Board.Pawn
                     IsWhite = true } -> [ imgWp ]
            | Some { PieceType = Board.King
                     IsWhite = false } -> [ imgBk ]
            | Some { PieceType = Board.Queen
                     IsWhite = false } -> [ imgBq ]
            | Some { PieceType = Board.Rook
                     IsWhite = false } -> [ imgBr ]
            | Some { PieceType = Board.Bishop
                     IsWhite = false } -> [ imgBb ]
            | Some { PieceType = Board.Knight
                     IsWhite = false } -> [ imgBn ]
            | Some { PieceType = Board.Pawn
                     IsWhite = false } -> [ imgBp ]
            | None -> []
            |> LazyList.ofList
            |> LazyList.map (fun v ->
                let r = 7 - rowIndex

                let c =
                    if areCoordsEnabled then columnIndex + 1 else columnIndex

                v.Row(r).Column(c)))
        |> LazyList.concat)
    |> LazyList.concat

let grid (dispatch: Msg.Msg -> unit) (areCoordsEnabled: bool) (boardSize: float) (board: Board.Board): ViewElement =
    let maxColumn = if areCoordsEnabled then 8 else 7

    let sizeNom =
        Device.info.PixelScreenSize.Width
        |> min Device.info.PixelScreenSize.Height
        |> (*) boardSize

    let sizeDenom = 2.0 * (float maxColumn + 1.0)
    let squareSize = sizeNom / sizeDenom

    View.Grid
        (columnSpacing = 0.0,
         rowSpacing = 0.0,
         rowdefs = [ for i in 0 .. maxColumn -> Dimension.Absolute squareSize ],
         coldefs = [ for i in 0 .. maxColumn -> Dimension.Absolute squareSize ],
         horizontalOptions = LayoutOptions.Center,
         gestureRecognizers =
             [ View.PanGestureRecognizer
                 (touchPoints = 1,
                  panUpdated =
                      (fun panArgs ->
                          match (panArgs.StatusType = GestureStatus.Running, panArgs.TotalX > 0.0) with
                          | (true, true) -> dispatch Msg.PanRightGesture
                          | (true, false) -> dispatch Msg.PanLeftGesture
                          | _ -> ())

                 ) ],
         children =
             (board
              |> pieces areCoordsEnabled
              |> LazyList.append (emptyBoard areCoordsEnabled)
              |> LazyList.toList))
