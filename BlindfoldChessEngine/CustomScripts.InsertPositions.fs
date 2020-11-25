module BlindfoldChessEngine.CustomScripts.InsertPositions

open Npgsql
open System.Data.Common

open BlindfoldChessMechanics.Notation
open BlindfoldChessMechanics.Logic.Game

let getNextUnparsedEvaluatedLineId(conn: NpgsqlConnection, trans: NpgsqlTransaction): int option =
    let sql = """
    SELECT id
    FROM evaluated_line
    WHERE id NOT IN (SELECT evaluated_line_id FROM position)
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
    (evaluated_line_id: int, num_of_halfmoves: int, fen: string)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql = "INSERT INTO position(evaluated_line_id, num_of_halfmoves, fen) VALUES(@evaluated_line_id, @num_of_halfmoves, @fen)"
    let f (cmd: NpgsqlCommand) =
        cmd.Parameters.AddWithValue("evaluated_line_id", evaluated_line_id) |> ignore
        cmd.Parameters.AddWithValue("num_of_halfmoves", num_of_halfmoves) |> ignore
        cmd.Parameters.AddWithValue("fen", fen) |> ignore
        cmd.Prepare()
    Utils.withTransactionalExecution f (conn, trans, sql)

let getNextUnparsedEvaluatedLineAndInsertFens(): unit
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        match getNextUnparsedEvaluatedLineId(conn, trans) with
        | None ->
            ()
        | Some lineId ->
            let (game: Game) = getEvaluatedLine lineId (conn, trans)
                               |> Parser.textOfGame
    ()

