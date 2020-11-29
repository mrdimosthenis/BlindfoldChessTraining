module BlindfoldChessEngine.CustomScripts.InsertEvals

open Npgsql
open System.Data.Common

open System.Text.RegularExpressions
open FSharpx.Collections


let parseEvals (text: string): string seq =
    let groupVal (i: int) (g: Group seq): string =
        (Seq.item i g).Value
    Regex.Matches(text, "\[%eval ([^\]]+)\]")
    |> Seq.cast
    |> Seq.map (fun (x:Match) -> x.Groups)
    |> Seq.map Seq.cast<Group>
    |> Seq.map (groupVal 1)

let getNextEvaluatedLineId(conn: NpgsqlConnection, trans: NpgsqlTransaction): int option =
    let sql = """
    SELECT id
    FROM evaluated_line
    WHERE is_parsed
        AND id NOT IN (SELECT DISTINCT(evaluated_line_id) FROM evaluation)
    LIMIT 1
    """
    let f (dr: DbDataReader) =
        if dr.Read()
            then dr.[0] :?> int |> Some
            else None
    Utils.withTransactionalQuery f (conn, trans, sql)

let insertSingleEval
    (evaluatedLineId: int, numOfHalfMoves: int, eval: string)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql = "INSERT INTO evaluation VALUES(@evaluated_line_id, @num_of_halfmoves, @eval)"
    let f (cmd: NpgsqlCommand) =
        cmd.Parameters.AddWithValue("evaluated_line_id", evaluatedLineId) |> ignore
        cmd.Parameters.AddWithValue("num_of_halfmoves", numOfHalfMoves) |> ignore
        cmd.Parameters.AddWithValue("eval", eval) |> ignore
        cmd.Prepare()
    Utils.withTransactionalExecution f (conn, trans, sql)

let getNextEvaluatedLineAndInsertEvals(): bool =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        match getNextEvaluatedLineId(conn, trans) with
        | None ->
            false
        | Some lineId ->
            InsertPositions.getEvaluatedLine lineId (conn, trans)
            |> parseEvals
            |> Seq.indexed
            |> Seq.map (fun (i, s) -> (i + 1, s))
            |> Seq.iter
                    (fun (i, eval) ->
                        insertSingleEval (lineId, i, eval) (conn, trans)
                    )
            true
    Utils.withDBTransaction f

let rec insertAllEvals(): bool =
    let res = getNextEvaluatedLineAndInsertEvals()
    if res then insertAllEvals() else res
