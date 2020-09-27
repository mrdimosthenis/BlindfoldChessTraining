module BlindfoldChessMechanics.Model.Logic.Position

exception InvalidMove

//let afterMove (move: Move) (board: Board): Board =
//    let (fromRowIndex, fromColumnIndex, toRowIndex, toColumnIndex) = move
//    let fromRow = Seq.item fromRowIndex board
//    let fromResident = Seq.item fromColumnIndex fromRow
//    let toRow = Seq.item toRowIndex board
//    let toResident = Seq.item toColumnIndex toRow
//    match (fromResident, toResident) with
//    | (None, _) | (Some {IsWhite=false}, Some {IsWhite=false}) |(Some {IsWhite=true}, Some {IsWhite=true}) -> raise InvalidMove
//    | _ -> let updatedFromRow = updatedSeq fromColumnIndex None fromRow
//           let updatedToRow = updatedSeq toColumnIndex fromResident toRow
//           board
//           |> updatedSeq fromRowIndex updatedFromRow
//           |> updatedSeq toRowIndex updatedToRow
