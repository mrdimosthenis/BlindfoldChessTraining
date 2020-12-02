module BlindfoldChessEngine.CustomScripts.Train

open Npgsql
open System.Data.Common

open FSharpx.Collections

open Synapses

open BlindfoldChessEngine.Fitting

let newNeuralNetworkJson(): string =
    [ 782; 600; 400; 200; 100; 3 ]
    |> NeuralNetwork.init
    |> NeuralNetwork.toJson

let learningRate = 0.005

let getLatestNetworkJson(conn: NpgsqlConnection, trans: NpgsqlTransaction): string =
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

let getRandomDataPoints (numOf: int) (conn: NpgsqlConnection, trans: NpgsqlTransaction): (string * string) List =
    let sql =
        sprintf """
                SELECT pos, ev
                FROM lichess_2020_10_sample
                ORDER BY RANDOM()
                LIMIT %i
                """
                numOf
    let f (dr: DbDataReader) =
        seq{ while dr.Read() do
                    yield (dr.[0] :?> string, dr.[1] :?> string) }
        |> List.ofSeq
    Utils.withTransactionalQuery f (conn, trans, sql)

let insertSingleNework
    (learning_rate: float, rmse: float, json_string: string)
    (conn: NpgsqlConnection, trans: NpgsqlTransaction): unit =
    let sql = "INSERT INTO synapses VALUES(@learning_rate, @rmse, @json_string)"
    let f (cmd: NpgsqlCommand) =
        cmd.Parameters.AddWithValue("learning_rate", learning_rate) |> ignore
        cmd.Parameters.AddWithValue("rmse", rmse) |> ignore
        cmd.Parameters.AddWithValue("json_string", json_string) |> ignore
        cmd.Prepare()
    Utils.withTransactionalExecution f (conn, trans, sql)

let encodedDatapoint(pos: string, eval: string): float list * float list =
    let x = pos |> Encode.fenFloats |> LazyList.toList
    let y = eval |> Encode.evaluationFloats |> LazyList.toList
    (x, y)

let fitNetworkAndInsert(): unit =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        let network = getLatestNetworkJson(conn, trans)
                      |> NeuralNetwork.ofJson
        let trainDatapoints =
                getRandomDataPoints 200 (conn, trans)
                |> LazyList.ofList
                |> LazyList.map encodedDatapoint
        let trainedNetwork =
            trainDatapoints
            |> LazyList.fold
                    (fun acc (x, y) ->
                        NeuralNetwork.fit(acc, learningRate, x, y)
                    )
                    network
        let testDatapoints =
            getRandomDataPoints 100 (conn, trans)
            |> LazyList.ofList
            |> LazyList.map encodedDatapoint
        let xs = LazyList.map fst testDatapoints
        let ys = LazyList.map snd testDatapoints
        let predictions =
            LazyList.map (fun x -> NeuralNetwork.prediction(trainedNetwork, x)) xs
        let rmse = LazyList.zip ys predictions
                   |> Statistics.rootMeanSquareError
        let trainedNetworkJson = NeuralNetwork.toJson trainedNetwork
        insertSingleNework(learningRate, rmse, trainedNetworkJson)(conn, trans)
    Utils.withDBTransaction f

let rec infiniteFit(): unit =
    fitNetworkAndInsert() |> ignore
    infiniteFit()
