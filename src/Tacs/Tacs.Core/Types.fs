namespace Tacs.Core

module Types =

    type BoundaryStrategy = 
        | Interpolated = 0
        | InsideInclusive = 1
        | InsideExclusive = 2
        | OutsideInclusive = 3
        | OutsideExclusive = 4

    type Remodel<'a> =
        | ToPoints
        | ToIntervals

    type Boundary<'a> = { location:'a; strategy:BoundaryStrategy }

    type IntervalSlice<'a> = { start:Boundary<'a>; ``end``:Boundary<'a>; }
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
        | None = 0
        | Custom = 1
        | Integral = 2
        | Avg = 3
        | Max = 4
        | Min = 5
        | Std = 6
        | Vrange = 7
        | Krange = 8

    type Aggregate<'a> = { windowing:Windowing<'a>; operation:AggregationOperation; config:string }

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
    
    type PointValue<'a, 'b> =  { position:'a; value:'b }

    type IntervalValue<'a, 'b> = { start:PointValue<'a,'b> option; ``end``:PointValue<'a,'b> option } //Intervals can be open to allow extrapolation

    // type Values<'a, 'b> =
    //     | PositionValues of PointValue<'a, 'b> list
    //     | IntervalValues of IntervalValue<'a, 'b> list

    type PointSequence<'a, 'b> = { id:string; interp:InterpolationStrategy; extrap:ExtrapolationStrategy; ptvalues:PointValue<'a,'b> list }

    type IntervalSequence<'a, 'b> = { id:string; interp:InterpolationStrategy; extrap:ExtrapolationStrategy; intvalues:IntervalValue<'a,'b> list }
