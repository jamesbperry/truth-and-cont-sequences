module Arguments

open Argu

type WindowSizeStrategy =
    | Width = 0
    | Count = 1

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
    | Plot = 4

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

type Anchor =
    | Start
    | End
    | Position of position:string

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
and IntervalsSampleArgs =
    | [<CliPrefix(CliPrefix.None)>] By of by:WindowSizeStrategy
    | [<CliPrefix(CliPrefix.None)>] Of of ofsize:string
    | [<CliPrefix(CliPrefix.None)>] AlignedTo of alignedTo:Anchor
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | By _ -> "Window sizing strategy."
            | Of _ -> "The window size."
            | AlignedTo _ -> "The point to fix as a window boundary."
and SampleArgs =
    | [<CliPrefix(CliPrefix.None)>] At of atOne:string
    | [<CliPrefix(CliPrefix.None)>] Many of atMany:string list
    | [<CliPrefix(CliPrefix.None)>] Intervals of ParseResults<IntervalsSampleArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | At _ -> "Sample at one specified position." 
            | Many _ -> "Sample at many specified positions." 
            | Intervals _ -> "Sample intervals of equal length or count." 
and SlidingWindowArgs = 
    | [<CliPrefix(CliPrefix.None)>] By of by:WindowSizeStrategy
    | [<CliPrefix(CliPrefix.None)>] Of of ``of``:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | By _ -> "Sizing strategy for the windows."
            | Of _ -> "Size of the windows."
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
and NonWindowedArgs =
    | [<Hidden>] Dummy
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Dummy -> "Not used. This enclosing type exists in contrast to Windowed, since nested types can't be optional."   
and AggregateArgs =
    | [<CliPrefix(CliPrefix.None)>] Windowed of ParseResults<WindowedArgs>
    | [<CliPrefix(CliPrefix.None)>] All of ParseResults<NonWindowedArgs>
    | [<AltCommandLine("-o")>] Operation of operation:AggregationOperations
    | [<AltCommandLine("-c")>] Config of config:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Windowed _ -> "Optional windowing strategy."
            | All _ -> ""
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
    | [<CliPrefix(CliPrefix.None)>] Remodel
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
            | Remodel _ -> "Transform a series of discrete points into a series of intervals, and vice versa."
            | Slice _ -> "Slice the input stream by minimum and/or maximum position values."
            | Aggregate _ -> "Aggregate the input stream."
            | Sample _ -> "Sample the input stream."
            | Compress _ -> "Compress the input stream."