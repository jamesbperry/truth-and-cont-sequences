namespace Tacs.Core

module FloatOps =

    open System
    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let FloatPosition (low:float) (high:float) (at:float) =
        ClampScale <| (at - low)/(high - low)           

    let InterpolatePosition (iv:Interval<float,'v,'i>) p : float =
        FloatPosition iv.startbound.position iv.endbound.position p

    let OnFloatPosition unitsize (ints:Interval<float,'v,'i> list) =
        List.map (fun int -> { interval=int;weight=(int.endbound.position - int.startbound.position) / unitsize}) ints

    let InterpolateValueConstant (v:float) (p:'p) =
        v
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,float>*PointValue<'p,float>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        pti.value + (dv * npos)

    type IFloatValue<'p> =
        inherit IIntervalValue<'p,float>
        abstract member Integral: float -> float
        abstract member Min: unit -> float
        abstract member Max: unit -> float
        abstract member Mean: unit -> float
        abstract member Range: unit -> float

    type FloatValuedInterval<'p> = Interval<'p,float,IFloatValue<'p>>
    type FloatValuedIntervalsNormalizer<'p> = float->FloatValuedInterval<'p> list->NormalizedInterval<'p,float,IFloatValue<'p>> list
    type FloatValuedSequence<'p> = IntervalSequence<'p,float,IFloatValue<'p>>

    type LinearFloatValue<'p> = 
        {pstart:PointValue<'p,float>;pend:PointValue<'p,float>} with
        interface IIntervalValue<'p,float> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,float>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid}) ////:> IIntervalValue<'p,float>
        interface IFloatValue<'p> with
            member this.Integral pweight = pweight * List.average [this.pstart.value;this.pend.value]
            member this.Min () = min this.pstart.value this.pend.value
            member this.Max () = max this.pstart.value this.pend.value
            member this.Mean () = List.average [this.pstart.value;this.pend.value]
            member this.Range () = abs <| this.pstart.value - this.pend.value

    let LinearFloatValue (pstart,pend) =
        {pstart=pstart;pend=pend}

    let LinearFloatInterval (startb:BoundaryValue<'p,float>,endb:BoundaryValue<'p,float>) =
        {startbound=startb.position;endbound=endb.position;value={LinearFloatValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb} :> IFloatValue<_>}      

    let Integral (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        let norms = np 1.0 inseq.intvalues
        let integs = Seq.map (fun nint -> (nint.interval.value :> IFloatValue<'p>).Integral nint.weight) norms
        let runningpairs = Seq.scan (+) 0.0 integs |> Seq.pairwise
        List.ofSeq <| Seq.map2 (fun i (sv,ev) -> LinearFloatInterval ({position=i.startbound;value=sv},{position=i.endbound;value=ev})) inseq.intvalues runningpairs
        
    let Average (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        failwith "not implemented"

    let Maximum (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        failwith "not implemented"

    let Minimum (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        failwith "not implemented"

    let Stdev (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        failwith "not implemented"

    let ValueRange (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        failwith "not implemented"