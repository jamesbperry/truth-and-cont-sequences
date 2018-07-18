namespace Tacs.Core

open System
module Types =

    type BoundaryStrategy =
        | InclusiveLow
        | InclusiveHigh

    type Inclusivity =
        | AsInclusive
        | AsExclusive

    type IntervalBoundary<'p> =
        | Inclusive of 'p
        | Exclusive of 'p
        static member private PositionOf ib =
            match ib with
            | Inclusive i -> i
            | Exclusive e -> e
        member self.position = IntervalBoundary<_>.PositionOf self
        static member (=@=) (x,y) =
            
            let xp = IntervalBoundary<_>.PositionOf x
            let yp = IntervalBoundary<_>.PositionOf y
            Object.Equals (xp,yp)

    type SliceStrategy = 
        | Inside
        | Interpolated 
        | Intersected

    type RemodelAnchor =
        | IntervalStart
        //| IntervalMidpoint //todo
        | IntervalEnd

    type Remodel =
        | ToPoints
        | ToIntervals

    type SliceBoundary<'p> = { boundary:IntervalBoundary<'p>; strategy:SliceStrategy }

    type IntervalSlice<'p> = { start:SliceBoundary<'p> option; endbound:SliceBoundary<'p> option; } //smells
    type ForwardSlice<'p> = { start:SliceBoundary<'p>; count:int; }
    type BackwardSlice<'p> = {count:int; endbound:SliceBoundary<'p>; }

    type Slice<'p> = 
        | IntervalSlice of IntervalSlice<'p>
        | ForwardSlice of ForwardSlice<'p>
        | BackwardSlice of BackwardSlice<'p>

    type Anchor<'p> =
        | Start //TODO rename to FromStart. My dotnet build is throwing a fit at time fo writing this comment
        | FromEnd
        | FromPosition of 'p

    type WindowingDirection =
        | LookingForward
        | LookingBackward

    type HoppingWindowing<'dp> = { size:'dp; hop:'dp }

    type Windowing<'p,'dp> = //Position and its difference can be different datatypes... e.g. TimeSpan for DateTime
        | Single
        | Tumbling of Anchor<'p> * WindowingDirection * 'dp
        | Hopping of Anchor<'p> * WindowingDirection * HoppingWindowing<'dp>
        | Sliding of WindowingDirection * 'dp

    type Aggregate<'p,'dp> = 
        | WholeSequence 
        | Windowed of Windowing<'p,'dp>

    type Sample<'p,'dp> =
        | SampleAt of 'p
        | SamplesAt of 'p list
        | SampleIntervals of 'dp * Anchor<'p>

    // type Operations<'p> =
    //     | Remodel of Remodel
    //     | Slice of Slice<'p>
    //     | Aggregate of Aggregate<'a>
    //     | Sample of Sample<'p,'dp>
    //     | Compress of Compress<'a>

    type ExtrapolationStrategy =
        | NoExtrapolation
        | BeforeFirst
        | AfterLast
        | BeforeAndAfter 

    type BoundaryValue<'p,'v> = { position:IntervalBoundary<'p>; value:'v }

    type PointValue<'p,'v> =  { position:'p; value:'v } with
        static member ofBoundary (bv:BoundaryValue<'p,'v>) =
            {position=bv.position.position;value=bv.value}
        static member map (f:'v -> 'v2) v : PointValue<'p,'v2> =
            {position=v.position;value=f v.value}       

    type NormalizedPosition = 
        | NormalizedPosition of float
        static member Start = NormalizedPosition 0.0
        static member End = NormalizedPosition 1.0

    type PositionNormalizer<'p> = 'p -> 'p -> 'p -> float
    type ValueInterpolator<'p,'v> = 'p -> 'v

    //We want .Split to return ('Tconcrete * 'Tconcrete). 
    //But F# lacks direct covariance and has a few other constraints, so it seems this is not possible in straightforward ways. 
    //Generic type constraints to interfaces do behave covariantly, though. We will abuse those.
    //By passing in a concrete IIntervalValue implementation as an argument to its own .Split instance, we capture its type as a 'generic constrained to implement IIntervalValue.
    //The concrete type becomes the return type of .Split, and after using a type match wrapped in a generic function to convert the output,
    //Voilà, covariance.
    type IIntervalValue<'p,'v> =
        abstract member At: PositionNormalizer<'p> -> 'p -> 'v
        abstract member Split: PositionNormalizer<'p> -> 'p -> 'i -> 'i * 'i when 'i :> IIntervalValue<'p,'v>

    //This is a bit of black magic used in allowing IIntervalValue.Split to return its concrete enclosing type
    //Poor man's dangerous downcast
    let asi (iiv:IIntervalValue<_,_>) : 'i = 
        match iiv with
        | :? 'i as ti -> ti
        | _ -> failwith "Failed to upcast interval value"

    let inline ClampScale (v:'a) = //TODO relocate
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    //TODO use inherited, not directly, so Split returns a properly-typed interval e.g. IFloatValue<_,_>
    type ConstantValue<'p,'v> =
        { value: 'v }
        interface IIntervalValue<'p,'v> with
            member this.At _ _ = this.value
            member this.Split _ _ self = 
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let thisi = asi this
                (thisi,thisi)

    let ConstantValue<'p,'v> v =
        { ConstantValue.value=v } :> IIntervalValue<'p,'v>         


    type IExtrapolation<'p,'v> =
        abstract member At: PositionNormalizer<'p> -> 'p -> 'v option


    type Interval<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> = { startbound:IntervalBoundary<'p>; endbound:IntervalBoundary<'p>; value:'i} with
        member this.ValueAt p = (this.value:> IIntervalValue<'p,'v>).At p

    type NormalizedInterval<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> = { interval:Interval<'p,'v,'i>; weight:float }

    type SplitInterval<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> = {before:Interval<'p,'v,'i> option;after:Interval<'p,'v,'i> option}  
    
    type InterpolationFunction<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> = Interval<'p,'v,'i> -> 'p -> PointValue<'p,'v>
    type AggregationFunction<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> = Interval<'p,'v,'i> seq -> Interval<'p,'v,'i>

    type PointSequence<'p, 'v> = { id:string; ptvalues:PointValue<'p,'v> list; extrap:ExtrapolationStrategy; bound:BoundaryStrategy}

    type IntervalSequence<'p, 'v, 'i when 'i :> IIntervalValue<'p,'v>> = { id:string; intvalues:Interval<'p,'v,'i> list; preextrap:IExtrapolation<'p,'v>; postextrap:IExtrapolation<'p,'v> }


    type Quality = Quality of string //placeholder
    type QualitiedValue<'v> = 'v * Quality list

    // type Angle = Angle of float
    // type Weight = Weight of float
    // type SplinePoint<'v> = 'v * Angle * Weight
    // type SplinedValue<'v> = SplinePoint<'v> * SplinePoint<'v> //note: not a LinearValue<SplinePoint<'v>> bc diff. interpretation strategy