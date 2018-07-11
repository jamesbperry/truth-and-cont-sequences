namespace Tacs.Core

module Types =

    type BoundaryStrategy =
        | InclusiveLow
        | InclusiveHigh

    type Inclusivity =
        | IsInclusive
        | IsExclusive

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

    type IntervalSlice<'p> = { start:SliceBoundary<'p> option; endbound:SliceBoundary<'p> option; }
    type ForwardSlice<'p> = { start:SliceBoundary<'p>; count:int; }
    type BackwardSlice<'p> = {count:int; endbound:SliceBoundary<'p>; }

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

    // type Operations<'p> =
    //     | Remodel of Remodel
    //     | Slice of Slice<'p>
    //     | Aggregate of Aggregate<'a>
    //     | Sample of Sample<'p,'dp>
    //     | Compress of Compress<'a>

    type InterpolationStrategy =
        | Step
        | Linear

    type ExtrapolationStrategy =
        | NoExtrapolation
        | BeforeFirst
        | AfterLast
        | BeforeAndAfter 

    type BoundaryValue<'p,'v> = { position:IntervalBoundary<'p>; value:'v }

    type PointValue<'p,'v> =  { position:'p; value:'v } with
        static member OfBoundary (bv:BoundaryValue<'p,'v>) =
            {position=bv.position.position;value=bv.value}

    type NormalizedPosition = 
        | NormalizedPosition of float
        static member Start = NormalizedPosition 0.0
        static member End = NormalizedPosition 1.0

    type PositionNormalizer<'p> = 'p -> 'p -> 'p -> float
    type ValueInterpolator<'p,'v> = 'p -> 'v

    type IIntervalValue<'p,'v> =
        abstract member At: PositionNormalizer<'p> -> 'p -> 'v
        abstract member Split: PositionNormalizer<'p> -> 'p -> IIntervalValue<'p,'v> * IIntervalValue<'p,'v>

    type IExtrapolation<'p,'v> =
        abstract member At: PositionNormalizer<'p> -> 'p -> 'v option

    type ConstantValue<'p,'v> =
        { value: 'v }
        interface IIntervalValue<'p,'v> with
            member this.At _ _ = this.value
            member this.Split _ _ = (this :> IIntervalValue<'p,'v>,this :> IIntervalValue<'p,'v>)

    let ConstantValue<'p,'v> v =
        { ConstantValue.value=v } :> IIntervalValue<'p,'v>         

    type Interval<'p,'v> = { startbound:IntervalBoundary<'p>; endbound:IntervalBoundary<'p>; value:IIntervalValue<'p,'v>}
    
    type SplitInterval<'p,'v> = {before:Interval<'p,'v> option;after:Interval<'p,'v> option}  
    
    type InterpolationFunction<'p,'v> = Interval<'p,'v> -> 'p -> PointValue<'p,'v>
    type AggregationFunction<'p,'v> = Interval<'p,'v> seq -> Interval<'p,'v>

    type Quality = Quality of string //placeholder
    type QualitiedValue<'v> = 'v * Quality list

    // type Angle = Angle of float
    // type Weight = Weight of float
    // type SplinePoint<'v> = 'v * Angle * Weight
    // type SplinedValue<'v> = SplinePoint<'v> * SplinePoint<'v> //note: not a LinearValue<SplinePoint<'v>> bc diff. interpretation strategy

    // type IVInterp<'p,'v> =
    //     abstract member At: 'p -> 'v

    // type FLinterp<'p> = { start:PointValue<'p,float>;endbound:PointValue<'p,float> } with
    //     interface IVInterp<'p,float> with
    //         member this.At pt = 42.0    

    // [<CustomEquality;NoComparison>]
    // type FintInterp =
    //     inherit ValueInterpolator<int,int>
    //     override this.Invoke p = 42
    //     override this.Equals(other) =
    //         match other with
    //         | :? FintInterp as tother -> this.Equals(tother)
    //         | _ -> false
    //     interface System.IEquatable<FintInterp> with
    //         member this.Equals other = true; //LOL
    //TODO move this...
    let Interpolate (pinterp) (vinterp) (iv:Interval<'p,'v>) (p:'p) : PointValue<'p,'v> =
        let s = 1.0 * pinterp iv p
        let v = vinterp iv s
        {position=p;value=v}    

    // let InterpolateValuePositiveStep (iv:FiniteIntervalValue<'p,'v>) (scale:float) : 'v =
    //     iv.start.value
 
    // let InterpolateValueNegativeStep (iv:FiniteIntervalValue<'p,'v>) (scale:float) : 'v =
    //     iv.endbound.value