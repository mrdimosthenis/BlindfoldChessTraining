module BlindfoldChessEngine.CustomScripts.Train

open Npgsql
open System.Data.Common

open FSharpx.Collections

open Synapses

open BlindfoldChessEngine.Fitting

let newNeuralNetworkJson(): string =
    [782; 64; 64; 64; 64; 64; 64; 64; 64; 3]
    |> NeuralNetwork.init
    |> NeuralNetwork.toJson

let learningRate = 1.0

let getLastNetworkJson(conn: NpgsqlConnection, trans: NpgsqlTransaction): string =
    let sql = """
    SELECT json_string
    FROM synapses
    ORDER BY tm DESC
    LIMIT 1
    """
    let f (dr: DbDataReader) =
        dr.Read() |> ignore
        dr.[0] :?> string
    Utils.withTransactionalQuery f (conn, trans, sql)

let get100RandomDataPoints (conn: NpgsqlConnection, trans: NpgsqlTransaction): (string * string) List =
    let sql = """
    SELECT pos, ev
    FROM lichess_2020_10_sample
    ORDER BY RANDOM()
    LIMIT 100
    """
    let f (dr: DbDataReader) =
        seq{ while dr.Read() do
                    yield (dr.[0] :?> string, dr.[1] :?> string) }
        |> List.ofSeq
    Utils.withTransactionalQuery f (conn, trans, sql)

let insertNeworkAndRmse
    (json_string: string, rmse_before_fit: float)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql = "INSERT INTO synapses VALUES(@json_string, @rmse_before_fit)"
    let f (cmd: NpgsqlCommand) =
        cmd.Parameters.AddWithValue("json_string", json_string) |> ignore
        cmd.Parameters.AddWithValue("rmse_before_fit", rmse_before_fit) |> ignore
        cmd.Prepare()
    Utils.withTransactionalExecution f (conn, trans, sql)

let fitNetworkAndInsert(): unit =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        let network = getLastNetworkJson(conn, trans)
                      |> NeuralNetwork.ofJson
        let datapoints =
                get100RandomDataPoints(conn, trans)
                |> LazyList.ofList
                |> LazyList.map
                       (fun (p, e) ->
                           let x = p |> Encode.fenFloats |> LazyList.toList
                           let y = e |> Encode.evaluationFloats |> LazyList.toList
                           (x, y)
                       )
        let xs = LazyList.map fst datapoints
        let ys = LazyList.map snd datapoints
        let predictions =
            LazyList.map (fun x -> NeuralNetwork.prediction(network, x)) xs
        let rmse_before_fit = LazyList.zip ys predictions
                              |> Statistics.rootMeanSquareError
        let trainedNetworkJson =
                datapoints
                |> LazyList.fold
                        (fun acc (x, y) ->
                            NeuralNetwork.fit(acc, learningRate, x, y)
                        )
                        network
                |> NeuralNetwork.toJson
        insertNeworkAndRmse(trainedNetworkJson, rmse_before_fit)(conn, trans)
    Utils.withDBTransaction f

let rec infiniteFit(): unit =
    fitNetworkAndInsert() |> ignore
    infiniteFit()
