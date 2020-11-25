module BlindfoldChessEngine.CustomScripts.Utils

open Npgsql

let withDBConnection<'a> (f: NpgsqlConnection -> 'a) : 'a =
    let cs = "Host=localhost;Username=postgres;Password=postgres;Database=postgres"
    let conn = new NpgsqlConnection(cs)
    conn.Open()
    let res = f conn
    conn.Close()
    res
