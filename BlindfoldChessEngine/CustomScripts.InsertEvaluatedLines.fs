module BlindfoldChessEngine.CustomScripts.InsertEvaluatedLines

open Npgsql
open System.IO

let connection(): NpgsqlConnection =
    let cs = "Host=localhost;Username=postgres;Password=postgres;Database=postgres";
    new NpgsqlConnection(cs)

let getOne(): string =
    let sql = "SELECT 1"
    let f conn =
        let cmd = new NpgsqlCommand(sql, conn)
        cmd.ExecuteScalar().ToString()
    Utils.withDBConnection f

let insert(): unit =
    let conn = connection()
    @"C:\Users\MrDIM\Desktop\lichess_db_evaluated_lines_202010.txt"
    |> File.ReadLines
    |> Seq.filter (fun s -> s.Trim() <> "")
    |> Seq.indexed
    |> Seq.iter
        (fun (i, s) ->
            let f conn =
                let sql = "INSERT INTO evaluated_line(i, month, line) VALUES(@i, @month, @line)"
                let cmd = new NpgsqlCommand(sql, conn)
                cmd.Parameters.AddWithValue("i", i) |> ignore
                cmd.Parameters.AddWithValue("month", "202010") |> ignore
                cmd.Parameters.AddWithValue("line", s) |> ignore
                cmd.Prepare()
                cmd.ExecuteNonQuery()
            Utils.withDBConnection f |> ignore
        )
