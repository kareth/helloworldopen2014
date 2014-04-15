open System
open System.IO
open System.Net.Sockets
open FSharp.Data

type Json = JsonProvider<"./sample.json", SampleIsList=true, RootName="message">

let send (msg : Json.Message) (writer : StreamWriter) =
    writer.WriteLine (msg.JsonValue.ToString(JsonSaveOptions.DisableFormatting))
    writer.Flush()

let join name key color =
    Json.Message(msgType = "join", data = Json.DecimalOrData(Json.Data(name = Some(name), key = Some(key), color = Some(color))))

let throttle (value : Decimal) =
    Json.Message(msgType = "throttle", data = Json.DecimalOrData(value))

let ping =
    Json.Message(msgType = "ping", data = null)

[<EntryPoint>]
let main args =
    let (host, portString, botName, botKey) = 
      args |> (function [|a;b;c;d|] -> a, b, c, d | _ -> failwith "Invalid param array")
    let port = Convert.ToInt32(portString)

    printfn "Connecting to %s:%d as %s/%s" host port botName botKey

    use client = new TcpClient(host, port)
    let stream = client.GetStream()
    let reader = new StreamReader(stream)
    let writer = new StreamWriter(stream)

    send (join botName botKey "blue") writer

    let processNextLine nextLine =
        match nextLine with
        | null -> false
        | line ->
            try 
                let msg = Json.Parse(line)
                match msg.MsgType with
                | "carPositions" -> send (throttle 0.5m) writer
                | "join" -> printfn "Joined"; send (ping) writer
                | "gameInit" -> printfn "Race init"; send (ping) writer
                | "gameEnd" -> printfn "Race ends"; send (ping) writer
                | "gameStart" -> printfn "Race starts"; send (ping) writer
                | _ -> send (ping) writer
                true
            with
            | ex -> true

    while processNextLine (reader.ReadLine()) do 
        ()

    0