module Arguments

open Argu

type CompressionStrategy =
    | None = 0
    | Custom = 1
    | SwingingDoor  = 2
    | LCA = 3

type AggregationOperations =
    | None = 0
    | Custom = 1
    | Integral = 2
    | Avg = 3
    | Max = 4
    | Min = 5
    | Std = 6
    | Vrange = 7
    | Krange = 8

[<CliPrefix(CliPrefix.Dash)>]
type CompressArgs =
    | [<AltCommandLine("-s")>] Strategy of strategy:CompressionStrategy
    | [<Last;AltCommandLine("-c")>] Config of config:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Strategy _ -> "Compression strategy to use."
            | Config _ -> "Parameters for the compression strategy."
and SampleArgs =
    | [<AltCommandLine("-todo")>] ToDo
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ToDo -> "Not yet implemented."
and AggregateArgs =
    | [<AltCommandLine("-o")>] Operation of operation:AggregationOperations
    | [<Last;AltCommandLine("-c")>] Config of config:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Operation _ -> "The operation to be applied."
            | Config _ -> "Parameters for the aggregation operation."
and SliceArgs =
    | [<AltCommandLine("-s")>] Start of start:string option
    | [<AltCommandLine("-c")>] Count of count:int option
    | [<AltCommandLine("-e")>] End of ``end``:string option
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Start _ -> "Truncate the beginning of the input stream."
            | Count _ -> "Return only this number of values. Positive number counts from start; negative counts from end."
            | End _ -> "Truncate the end of the input stream."
and TacsArgs =
    | [<AltCommandLine("-i")>] Input of input:string option
    | [<AltCommandLine("-o")>] Output of output:string option
    | [<CliPrefix(CliPrefix.None)>] Slice of ParseResults<SliceArgs>
    | [<CliPrefix(CliPrefix.None)>] Aggregate of ParseResults<AggregateArgs>
    | [<CliPrefix(CliPrefix.None)>] Sample of ParseResults<SampleArgs>
    | [<CliPrefix(CliPrefix.None)>] Compress of ParseResults<CompressArgs>
with
    interface IArgParserTemplate with
        member this.Usage = 
            match this with
            | Input _ -> "Read input stream from a file. If not specified, expects stdin."
            | Output _ -> "Write resultant stream to a file. If not specified, writes to stdout."
            | Slice _ -> "Slice the input stream by minimum and/or maximum position values."
            | Aggregate _ -> "Aggregate the input stream."
            | Sample _ -> "Sample the input stream."
            | Compress _ -> "Compress the input stream."