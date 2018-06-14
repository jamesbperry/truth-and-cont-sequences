namespace Tacs.Core

module Types =

    // type BoundaryType =
    //     | Inclusive
    //     | Exclusive

    //type IntervalBoundary<'p> = { value:'p; nature:BoundaryType }

    type BoundaryStrategy =
        | InclusiveLow
        | InclusiveHigh

    type IntervalBoundary<'p> =
        | Inclusive of 'p
        | Exclusive of 'p
        member self.position =
            match self with
            | Inclusive i -> i
            | Exclusive e -> e

    type SliceStrategy = 
        | Inside //Exclusive
        | Interpolated
        | Intersected //Inclusive

    type RemodelAnchor =
        | IntervalStart
        //| IntervalMidpoint //todo
        | IntervalEnd

    type Remodel =
        | ToPoints
        | ToIntervals

    type SliceBoundary<'p> = { position:'p; strategy:SliceStrategy }

    type IntervalSlice<'p> = { start:SliceBoundary<'p> option; ``end``:SliceBoundary<'p> option; }
    type ForwardSlice<'p> = { start:SliceBoundary<'p>; count:int; }
    type BackwardSlice<'p> = {count:int; ``end``:SliceBoundary<'p>; }

    type Slice<'p> = 
        | IntervalSlice of IntervalSlice<'p>
        | ForwardSlice of ForwardSlice<'p>
        | BackwardSlice of BackwardSlice<'p>

    type IntervalSize<'dp> = //But can be a different datatype than 'p... e.g. TimeSpan for DateTime
        | Width of 'dp
        | Count of int

    type HoppingWindowing<'dp> = { size:IntervalSize<'dp>; hop:IntervalSize<'dp> }

    type Windowing<'dp> =
        | Single
        | Sliding of IntervalSize<'dp>
        | Hopping of HoppingWindowing<'dp>

    type AggregationOperation = //TODO remove. types define their own aggregators!
        | NoOp
        | Custom of string
        | Integral
        | Avg
        | Max
        | Min
        | Std
        | Range

    type Aggregate<'dp> = { windowing:Windowing<'dp>; operation:AggregationOperation; } //TODO rework. Types define their own aggregators!

    type Anchor<'p> =
        | Start
        | End
        | Position of 'p

    type Sample<'p,'dp> =
        | Point of 'p
        | Points of 'p list
        | Intervals of IntervalSize<'dp> * Anchor<'p>

    type CompressionStrategy = //Shouldn't types define compression strategy too?
        | None = 0
        | Custom = 1
        | SwingingDoor  = 2
        | LCA = 3
        | Plot = 4

    type Compress<'a> = { strategy:CompressionStrategy; config:string } //Work out the typing on these

    // type Operations<'p> =
    //     | Remodel of Remodel
    //     | Slice of Slice<'p>
    //     | Aggregate of Aggregate<'a>
    //     | Sample of Sample<'p,'dp>
    //     | Compress of Compress<'a>

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

    type FiniteIntervalValue<'p,'v> = { start:IntervalBoundary<'p>; ``end``:IntervalBoundary<'p>; value:'p->'v}
    type ForwardRayIntervalValue<'p,'v> = { start:IntervalBoundary<'p>; value:'p->'v}
    type BackwardRayIntervalValue<'p,'v> = { ``end``:IntervalBoundary<'p>; value:'p->'v}
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