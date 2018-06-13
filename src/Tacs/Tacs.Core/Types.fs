namespace Tacs.Core

module Types =

    type BoundaryStrategy = 
        | Inside
        | Interpolated
        | Intersected

    type RemodelAnchor =
        | IntervalStart
        //| IntervalMidpoint //todo
        | IntervalEnd

    type Remodel<'a> =
        | ToPoints
        | ToIntervals

    type Boundary<'a> = { location:'a; strategy:BoundaryStrategy }

    type IntervalSlice<'a> = { start:Boundary<'a> option; ``end``:Boundary<'a> option; }
    type ForwardSlice<'a> = { start:Boundary<'a>; count:int; }
    type BackwardSlice<'a> = {count:int; ``end``:Boundary<'a>; }

    type Slice<'a> = 
        | IntervalSlice of IntervalSlice<'a>
        | ForwardSlice of ForwardSlice<'a>
        | BackwardSlice of BackwardSlice<'a>

    type IntervalSize<'a> =
        | Width of 'a
        | Count of int

    type HoppingWindowing<'a> = { size:IntervalSize<'a>; hop:IntervalSize<'a> }

    type Windowing<'a> =
        | Single
        | Sliding of IntervalSize<'a>
        | Hopping of HoppingWindowing<'a>

    type AggregationOperation =
        | NoOp
        | Custom of string
        | Integral
        | Avg
        | Max
        | Min
        | Std
        | Range

    type Aggregate<'a> = { windowing:Windowing<'a>; operation:AggregationOperation; }

    type Anchor<'a> =
        | Start
        | End
        | Position of 'a

    type Sample<'a> =
        | Point of 'a
        | Points of 'a list
        | Intervals of IntervalSize<'a> * Anchor<'a>

    type CompressionStrategy =
        | None = 0
        | Custom = 1
        | SwingingDoor  = 2
        | LCA = 3
        | Plot = 4

    type Compress<'a> = { strategy:CompressionStrategy; config:string }    

    type Operations<'a> =
        | Remodel of Remodel<'a>
        | Slice of Slice<'a>
        | Aggregate of Aggregate<'a>
        | Sample of Sample<'a>
        | Compress of Compress<'a>

    type InterpolationStrategy =
        | Step
        | Linear

    [<System.FlagsAttribute>]
    type ExtrapolationStrategy =
        | NoExtrapolation = 0
        | BeforeFirst = 1
        | AfterLast = 2
        | BeforeAndAfter = 3   

    type PointValue<'p,'v> =  { position:'p; value:'v }

    type NormalizedPosition = 
        | NormalizedPosition of float
        static member Start = NormalizedPosition 0.0
        static member End = NormalizedPosition 1.0

    type FiniteIntervalValue<'p,'v> = { start:'p; ``end``:'p; value:'p->'v}
    type ForwardRayIntervalValue<'p,'v> = { start:'p; value:'p->'v}
    type BackwardRayIntervalValue<'p,'v> = { ``end``:'p; value:'p->'v}
    type InstantaneousIntervalValue<'p,'v> = { instant:'p; value:'p->'v}

    type IntervalValue<'a,'b> =
        | FiniteIntervalValue of FiniteIntervalValue<'a,'b>
        | ForwardRayIntervalValue of ForwardRayIntervalValue<'a,'b>
        | BackwardRayIntervalValue of BackwardRayIntervalValue<'a,'b>
        | InstantaneousIntervalValue of InstantaneousIntervalValue<'a,'b>

    type SplitIntervalValue<'p,'v> = {before:IntervalValue<'p,'v> option;after:IntervalValue<'p,'v> option}  
    
    type InterpolationFunction<'p,'v> = FiniteIntervalValue<'p,'v> -> 'p -> PointValue<'p,'v>
    type AggregationFunction<'p,'v> = IntervalValue<'p,'v> seq -> IntervalValue<'p,'v>


    type Quality = Quality of string //placeholder
    type QualitiedValue<'v> = 'v * Quality list

    // type Angle = Angle of float
    // type Weight = Weight of float
    // type SplinePoint<'v> = 'v * Angle * Weight
    // type SplinedValue<'v> = SplinePoint<'v> * SplinePoint<'v> //note: not a LinearValue<SplinePoint<'v>> bc diff. interpretation strategy

    type PositionInterpolator<'p,'v> = FiniteIntervalValue<'p,'v> -> 'p -> float
    type PositionNormalizer<'p> = 'p -> 'p -> 'p -> float

    //TODO move this...
    let Interpolate (pinterp) (vinterp) (iv:FiniteIntervalValue<'p,'v>) (p:'p) : PointValue<'p,'v> =
        let s = 1.0 * pinterp iv p
        let v = vinterp iv s
        {position=p;value=v}    

    // let InterpolateValuePositiveStep (iv:FiniteIntervalValue<'p,'v>) (scale:float) : 'v =
    //     iv.start.value
 
    // let InterpolateValueNegativeStep (iv:FiniteIntervalValue<'p,'v>) (scale:float) : 'v =
    //     iv.``end``.value