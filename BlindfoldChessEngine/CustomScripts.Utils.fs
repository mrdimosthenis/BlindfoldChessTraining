module BlindfoldChessEngine.CustomScripts.Utils

open Npgsql
open System.Data.Common

let withDBConnection<'a> (f: NpgsqlConnection -> 'a) : 'a =
    let cs = "Host=localhost;Username=postgres;Password=postgres;Database=postgres"
    let conn = new NpgsqlConnection(cs)
    conn.Open()
    let res = f conn
    conn.Close()
    res

let withDBTransaction<'a> (f: NpgsqlConnection * NpgsqlTransaction -> 'a) : 'a =
    let g (conn: NpgsqlConnection) =
        let transaction = conn.BeginTransaction()
        let res = f (conn, transaction)
        transaction.Commit()
        res
    withDBConnection g

let withTransactionalQuery<'a>
        (f: DbDataReader -> 'a)
        (conn: NpgsqlConnection, trans: NpgsqlTransaction, sql: string): 'a =
    let cmd = new NpgsqlCommand()
    cmd.Connection <- conn
    cmd.Transaction <- trans
    cmd.CommandText <- sql
    let dr = cmd.ExecuteReader()
    let res = f dr
    dr.Dispose()
    res

let withTransactionalExecution<'a>
    (f: NpgsqlCommand -> unit)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction, sql: string): unit =
    let cmd = new NpgsqlCommand()
    cmd.Connection <- conn
    cmd.Transaction <- trans
    cmd.CommandText <- sql
    f cmd
    cmd.ExecuteNonQuery() |> ignore
