module BlindfoldChessEngine.CustomScripts.InsertPositions

open Npgsql
open System.Data.Common

open BlindfoldChessMechanics
open BlindfoldChessMechanics.Notation
open BlindfoldChessMechanics.Logic.Game
open BlindfoldChessMechanics.Logic

let getNextUnparsedEvaluatedLineId(conn: NpgsqlConnection, trans: NpgsqlTransaction): int option =
    let sql = """
    SELECT id
    FROM evaluated_line
    WHERE is_parsed = FALSE
    LIMIT 1
    """
    let f (dr: DbDataReader) =
        if dr.Read()
            then dr.[0] :?> int |> Some
            else None
    Utils.withTransactionalQuery f (conn, trans, sql)

let getEvaluatedLine (id: int) (conn: NpgsqlConnection, trans: NpgsqlTransaction): string =
    let sql =
        sprintf """
                SELECT line
                FROM evaluated_line
                WHERE id = %i
                """
                id
    let f (dr: DbDataReader) =
        dr.Read() |> ignore
        dr.[0] :?> string
    Utils.withTransactionalQuery f (conn, trans, sql)

let insertSingleFen
    (evaluatedLineId: int, numOfHalfMoves: int, fen: string)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql = "INSERT INTO position VALUES(@evaluated_line_id, @num_of_halfmoves, @fen)"
    let f (cmd: NpgsqlCommand) =
        cmd.Parameters.AddWithValue("evaluated_line_id", evaluatedLineId) |> ignore
        cmd.Parameters.AddWithValue("num_of_halfmoves", numOfHalfMoves) |> ignore
        cmd.Parameters.AddWithValue("fen", fen) |> ignore
        cmd.Prepare()
    Utils.withTransactionalExecution f (conn, trans, sql)

let markEvaluationLineAsParsed (id: int) (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql =
        sprintf """
                UPDATE evaluated_line
                SET is_parsed = TRUE
                WHERE id = %i
                """
                id
    let f (cmd: NpgsqlCommand) = ()
    Utils.withTransactionalExecution f (conn, trans, sql)

let getNextUnparsedEvaluatedLineAndInsertFens(): bool =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        match getNextUnparsedEvaluatedLineId(conn, trans) with
        | None ->
            false
        | Some lineId ->
            let game = getEvaluatedLine lineId (conn, trans)
                       |> Parser.textOfGame
            game.Moves
            |> Seq.ofArray
            |> Seq.fold
                    (fun acc x ->
                        let nextPos = acc
                                      |> Seq.head
                                      |> Position.positionAfterMove x
                        Seq.append (Seq.ofList [ nextPos ]) acc
                    )
                    (Seq.ofList [ Position.init ])
            |> Seq.rev
            |> Seq.indexed
            |> Seq.tail
            |> Seq.iter
                    (fun (i, pos) ->
                        let fen = Emitter.positionText pos
                        insertSingleFen (lineId, i, fen) (conn, trans)
                    )
            markEvaluationLineAsParsed lineId (conn, trans)
            true
    Utils.withDBTransaction f

let rec insertAllFens(): bool =
    let res = getNextUnparsedEvaluatedLineAndInsertFens()
    if res then insertAllFens() else res
