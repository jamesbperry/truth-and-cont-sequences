module Arguments

open Argu

type SliceStrategyCLI =
    | Inside = 0
    | Interpolated = 1
    | Intersected = 2

//type SliceBoundaryArgs =

type CountSliceArgs =
    | [<CliPrefix(CliPrefix.None);Mandatory>]As of SliceStrategyCLI
    | [<CliPrefix(CliPrefix.None);Mandatory>]From of from:string
    | [<CliPrefix(CliPrefix.None);Mandatory>]Count of count:int
with     
    interface IArgParserTemplate with
        member this.Usage =
            match this with
                | As _ -> "How the boundary is enforced."
                | From _ -> "Position for the slice."
                | Count _ -> "Number of values to include."

//type IntervalSliceArgs =

type SliceArgs =
    //| Interval of ParseResults<IntervalSliceArgs>
    | [<CliPrefix(CliPrefix.None)>]Forward of ParseResults<CountSliceArgs>
    | [<CliPrefix(CliPrefix.None)>]Backward of ParseResults<CountSliceArgs>
with     
    interface IArgParserTemplate with
        member this.Usage =
            match this with
                | Forward _ -> "Slice forward, i.e. with increasing position."
                | Backward _ -> "Slice backward, i.e. with decreasing position."

type BuiltInIntInterp =
    | Constant = 0
    | LinearNearest = 1
    | LinearFloor = 2
    | LinearCeiling = 3

type BuiltInFloatInterp =
    | Constant = 0
    | Linear = 1

type BuiltInTimeInterp =
    | Constant = 0
    | Linear = 1

type BuiltInValueTypes =
    | [<CliPrefix(CliPrefix.None)>]Integer of BuiltInIntInterp
    | [<CliPrefix(CliPrefix.None)>]Float of BuiltInFloatInterp
    | [<CliPrefix(CliPrefix.None)>]Time of BuiltInTimeInterp
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Integer _ -> "Values are integers."
            | Float _ -> "Values are floats."
            | Time _ -> "Values are datetimes."

type BuiltInPositionTypes =
    | Integer = 0
    | Float = 1
    | Time = 2

type IntervalSide =
    | BeginningOfInterval = 0
    | EndOfInterval = 1

type RemodelToPointsArgs =
    | [<CliPrefix(CliPrefix.None);Mandatory;Unique>] WithPositionType of BuiltInPositionTypes 
    // | [<CliPrefix(CliPrefix.None);Mandatory;Unique>] TimestampedAt of IntervalSide
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | WithPositionType _ -> "The data type of the sequence's position, i.e. x-axis."
            // | TimestampedAt_ -> "Each interval becomes one point. This is which side the timestamp."

type RemodelToIntervalsArgs =
    | [<CliPrefix(CliPrefix.None);Mandatory;Unique>] WithPositionType of BuiltInPositionTypes 
    | [<CliPrefix(CliPrefix.None)>] AndValueType of ParseResults<BuiltInValueTypes>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | WithPositionType _ -> "The data type of the sequence's position, i.e. x-axis."
            | AndValueType _ -> "The value type and interpolation scheme of the sequence's value, i.e. y-axis"

type RemodelArgs =
    | [<CliPrefix(CliPrefix.None)>] ToPoints of ParseResults<RemodelToPointsArgs>
    | [<CliPrefix(CliPrefix.None)>] ToIntervals of ParseResults<RemodelToIntervalsArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ToPoints _ -> "The data type of the sequence's position, i.e. x-axis."
            | ToIntervals _ -> "The value type and interpolation scheme of the sequence's value, i.e. y-axis"

type CompressArgs = 
    | Placeholder
with
    interface IArgParserTemplate with
        member __.Usage = "To do"

type SampleArgs = 
    | Placeholder
with
    interface IArgParserTemplate with
        member __.Usage = "To do"

type AggregateArgs = 
    | Placeholder
with
    interface IArgParserTemplate with
        member __.Usage = "To do"

type ProjectArgs = 
    | Placeholder
with
    interface IArgParserTemplate with
        member __.Usage = "To do"

type TacsArgs =
    | [<AltCommandLine("-i")>] Input of input:string option
    | [<AltCommandLine("-o")>] Output of output:string option
    | [<CliPrefix(CliPrefix.None)>] Remodel of ParseResults<RemodelArgs>
    | [<CliPrefix(CliPrefix.None)>] Slice of ParseResults<SliceArgs>
    | [<CliPrefix(CliPrefix.None)>] Project of ParseResults<ProjectArgs>
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
            | Project _ -> "Project the input stream values into a different type. (Map)"
            | Aggregate _ -> "Aggregate the input stream. (Reduce)"
            | Sample _ -> "Sample the input stream."
            | Compress _ -> "Compress the input stream."