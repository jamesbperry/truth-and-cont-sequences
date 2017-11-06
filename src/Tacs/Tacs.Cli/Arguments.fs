module Arguments

open Argu

type BoundaryStrategy =
    | Interpolated = 0
    | InsideInclusive = 1
    | InsideExclusive = 2
    | OutsideInclusive = 3
    | OutsideExclusive = 4

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
and CountSlidingWindowArgs =
    | [<AltCommandLine("-c")>] Count of count:int
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Count _ -> "The (maximum) number of events which will be included in each window."
and WidthSlidingWindowArgs =
    | [<AltCommandLine("-w")>] Width of width:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Width _ -> "The size of each window."    
and SlidingWindowArgs = 
    | [<CliPrefix(CliPrefix.None)>] ByCount of ParseResults<CountSlidingWindowArgs>
    | [<CliPrefix(CliPrefix.None)>] ByWidth of ParseResults<WidthSlidingWindowArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ByCount _ -> "Windows are sized by a maximum event count."
            | ByWidth _ -> "Windows are sized by a fixed width."
and HoppingWindowArgs = 
    | [<AltCommandLine("-w")>] Width of width:string
    | [<AltCommandLine("-cw")>] Hop of hop:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Width _ -> "Width of each window."
            | Hop _ -> "Windows are generated at this interval."
and WindowedArgs =
    | [<CliPrefix(CliPrefix.None)>] Sliding of ParseResults<SlidingWindowArgs>
    | [<CliPrefix(CliPrefix.None)>] Hopping of ParseResults<HoppingWindowArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Sliding _ -> "A window is generated for each event, with the event as the window end"
            | Hopping _ -> "Windows advance by a constant position value"
and AggregateArgs =
    | [<CliPrefix(CliPrefix.None)>] Windowing of ParseResults<WindowedArgs> option
    | [<AltCommandLine("-o")>] Operation of operation:AggregationOperations
    | [<Last;AltCommandLine("-c")>] Config of config:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Windowing _ -> "Optional windowing strategy."
            | Operation _ -> "The operation to be applied."
            | Config _ -> "Parameters for the aggregation operation."
and SliceArgs =
    | [<AltCommandLine("-s")>] Start of start:string option
    | [<AltCommandLine("-ss")>] StartStrategy of startstrategy:BoundaryStrategy option
    | [<AltCommandLine("-c")>] Count of count:int option
    | [<AltCommandLine("-e")>] End of ``end``:string option
    | [<AltCommandLine("-es")>] EndStrategy of endstrategy:BoundaryStrategy option
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Start _ -> "Truncate the beginning of the input stream."
            | StartStrategy _ -> "How the start point is enforced."
            | Count _ -> "Return only this number of values. Positive number counts from start; negative counts from end."
            | End _ -> "Truncate the end of the input stream."
            | EndStrategy _ -> "How the end point is enforced."
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