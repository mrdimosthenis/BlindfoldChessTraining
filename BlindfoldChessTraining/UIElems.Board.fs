module BlindfoldChessTraining.UIElems.Board

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Logic

exception InvalidRow of int

let imgElem (imgName: string): ViewElement =
    let pngImgSrc = imgName
                    |> sprintf "BlindfoldChessTraining.resources.images.board.%s.png"
                    |> ImageSource.FromResource
                    |> Image.fromImageSource
    View.Image(source = pngImgSrc)

let imgWh: ViewElement = imgElem "wh"
let img1: ViewElement = imgElem "1"
let imgBl: ViewElement = imgElem "bl"
let img2: ViewElement = imgElem "2"
let img3: ViewElement = imgElem "3"
let img4: ViewElement = imgElem "4"
let img5: ViewElement = imgElem "5"
let img6: ViewElement = imgElem "6"
let img7: ViewElement = imgElem "7"
let img8: ViewElement = imgElem "8"
let imgA: ViewElement = imgElem "a"
let imgB: ViewElement = imgElem "b"
let imgBb: ViewElement = imgElem "bb"
let imgBk: ViewElement = imgElem "bk"
let imgBn: ViewElement = imgElem "bn"
let imgBp: ViewElement = imgElem "bp"
let imgBq: ViewElement = imgElem "bq"
let imgBr: ViewElement = imgElem "br"
let imgBL: ViewElement = imgElem "b_l"
let imgBR: ViewElement = imgElem "b_r"
let imgC: ViewElement = imgElem "c"
let imgD: ViewElement = imgElem "d"
let imgE: ViewElement = imgElem "e"
let imgF: ViewElement = imgElem "f"
let imgG: ViewElement = imgElem "g"
let imgH: ViewElement = imgElem "h"
let imgR: ViewElement = imgElem "r"
let imgT: ViewElement = imgElem "t"
let imgTL: ViewElement = imgElem "t_l"
let imgTR: ViewElement = imgElem "t_r"
let imgWb: ViewElement = imgElem "wb"
let imgWk: ViewElement = imgElem "wk"
let imgWn: ViewElement = imgElem "wn"
let imgWp: ViewElement = imgElem "wp"
let imgWq: ViewElement = imgElem "wq"
let imgWr: ViewElement = imgElem "wr"

let topRowElems: ViewElement seq =
    [| [| imgTL |]
       Array.replicate 8 imgT
       [| imgTR |] |]
    |> Utils.seqOfArrays
    |> Seq.concat
    |> Seq.indexed
    |> Seq.map (fun (i, v) -> v.Row(0).Column(i))
    |> Seq.cache

let bottomRowElems: ViewElement seq =
    [| imgBL; imgA; imgB; imgC; imgD; imgE; imgF; imgG; imgH; imgBR |]
    |> Seq.ofArray
    |> Seq.indexed
    |> Seq.map (fun (i, v) -> v.Row(9).Column(i))
    |> Seq.cache

let emptyBoardRowElems (areCoordsEnabled: bool) (r: int): ViewElement seq =
    let innerRowElems = if r % 2 = 0 then [| imgBl; imgWh |]
                        else [| imgWh; imgBl |]
                        |> Seq.ofArray
                        |> Seq.replicate 4
                        |> Seq.concat
    if areCoordsEnabled
        then let fstElem = match r with
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
             |> Seq.rev
             |> Utils.prependedSeq imgR
             |> Seq.rev
             |> Utils.prependedSeq fstElem
             |> Seq.indexed
             |> Seq.map (fun (i, v) -> v.Row(8 - r).Column(i))
    else
        innerRowElems 
        |> Seq.indexed
        |> Seq.map (fun (i, v) -> v.Row(7 - r).Column(i))
    |> Seq.cache

let emptyBoardElems (areCoordsEnabled: bool): ViewElement seq =
    let middleRowElems = Seq.init 8 id
                         |> Seq.map (emptyBoardRowElems areCoordsEnabled)
                         |> Seq.concat
    if areCoordsEnabled
        then [| topRowElems; middleRowElems; bottomRowElems |]
    else [| middleRowElems |]
    |> Seq.ofArray
    |> Seq.concat
    |> Seq.cache

let pieceElems (areCoordsEnabled: bool) (board: Board.Board): ViewElement seq =
    board
    |> Utils.seqOfArrays
    |> Seq.indexed
    |> Seq.map
        (fun (rowIndex, row) ->
            row
            |> Seq.indexed
            |> Seq.map
                (fun (columnIndex, resident) ->
                    match resident with
                    | Some { PieceType = Board.King; IsWhite = true } ->
                        [| imgWk |]
                    | Some { PieceType = Board.Queen; IsWhite = true } ->
                        [| imgWq |]
                    | Some { PieceType = Board.Rook; IsWhite = true } ->
                        [| imgWr |]
                    | Some { PieceType = Board.Bishop; IsWhite = true } ->
                        [| imgWb |]
                    | Some { PieceType = Board.Knight; IsWhite = true } ->
                        [| imgWn |]
                    | Some { PieceType = Board.Pawn; IsWhite = true } ->
                        [| imgWp |]
                    | Some { PieceType = Board.King; IsWhite = false } ->
                        [| imgBk |]
                    | Some { PieceType = Board.Queen; IsWhite = false } ->
                        [| imgBq |]
                    | Some { PieceType = Board.Rook; IsWhite = false } ->
                        [| imgBr |]
                    | Some { PieceType = Board.Bishop; IsWhite = false } ->
                        [| imgBb |]
                    | Some { PieceType = Board.Knight; IsWhite = false } ->
                        [| imgBn |]
                    | Some { PieceType = Board.Pawn; IsWhite = false } ->
                        [| imgBp |]
                    | None -> [||]
                    |> Seq.ofArray
                    |> Seq.map
                        (fun v ->
                            let r = if areCoordsEnabled
                                        then 8 - rowIndex
                                    else
                                        7 - rowIndex
                            let c = if areCoordsEnabled
                                        then columnIndex + 1
                                    else
                                        columnIndex
                            v.Row(r).Column(c)
                        )
                )
            |> Seq.concat
        )
    |> Seq.concat
    |> Seq.cache

let squareSize (areCoordsEnabled: bool): float =
    Device.info.PixelScreenSize.Width
    |> min Device.info.PixelScreenSize.Height
    |> (*) (0.9 * if areCoordsEnabled then 0.05 else 0.0625)

let boardGrid (areCoordsEnabled: bool) (board: Board.Board): ViewElement =
    let absSize = squareSize areCoordsEnabled
    let maxColumn = if areCoordsEnabled then 9 else 7
    View.Grid(
        columnSpacing = 0.0,
        rowSpacing = 0.0,
        rowdefs = [for i in 0 .. maxColumn -> Dimension.Absolute absSize],
        coldefs = [for i in 0 .. maxColumn -> Dimension.Absolute absSize],
        horizontalOptions = LayoutOptions.Center,
        children = (board |> pieceElems areCoordsEnabled |> Seq.append (emptyBoardElems areCoordsEnabled) |> Seq.toList)
    )
