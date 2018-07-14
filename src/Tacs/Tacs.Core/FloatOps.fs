namespace Tacs.Core

open Sequence
open Sequence
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
        abstract member Total: float -> float
        abstract member Min: unit -> float
        abstract member Max: unit -> float
        abstract member Mean: unit -> float
        abstract member Range: unit -> float

    type FloatValuedInterval<'p> = Interval<'p,float,IFloatValue<'p>>
    type FloatValuedIntervalsNormalizer<'p> = float->FloatValuedInterval<'p> list->NormalizedInterval<'p,float,IFloatValue<'p>> list
    type FloatValuedSequence<'p> = IntervalSequence<'p,float,IFloatValue<'p>>

    type ConstantFloatValue<'p> =
        {constantvalue:float} with
        interface IIntervalValue<'p,float> with
            member this.At _ _ = this.constantvalue
            member this.Split _ _ _ = (asi this,asi this)
        interface IFloatValue<'p> with
            member this.Total pweight = pweight * this.constantvalue
            member this.Min () = this.constantvalue
            member this.Max () = this.constantvalue
            member this.Mean () = this.constantvalue
            member __.Range () = 0.0   
    
    let ConstantFloatValue (value) = {constantvalue=value}

    let ConstantFloatInterval (startb:IntervalBoundary<'p>,endb:IntervalBoundary<'p>) value =
        {startbound=startb;endbound=endb;value={ConstantFloatValue.constantvalue=value} :> IFloatValue<_>}      

    type LinearFloatValue<'p> = 
        {pstart:PointValue<'p,float>;pend:PointValue<'p,float>} with
        interface IIntervalValue<'p,float> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,float>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IFloatValue<'p> with
            member this.Total pweight = pweight * List.average [this.pstart.value;this.pend.value]
            member this.Min () = min this.pstart.value this.pend.value
            member this.Max () = max this.pstart.value this.pend.value
            member this.Mean () = List.average [this.pstart.value;this.pend.value]
            member this.Range () = abs <| this.pstart.value - this.pend.value

    let LinearFloatValue (pstart,pend) =
        {pstart=pstart;pend=pend}

    let LinearFloatInterval (startb:BoundaryValue<'p,float>,endb:BoundaryValue<'p,float>) =
        {startbound=startb.position;endbound=endb.position;value={LinearFloatValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb} :> IFloatValue<_>}      

    // type PiecewiseFloatValue<'p when 'p : comparison> =
    //     {segments:FloatValuedInterval<'p> list} with
    //     member private this._AsSequence () = {id="__";intvalues=this.segments;preextrap=Sequence.NoExtrapolation ();postextrap=Sequence.NoExtrapolation ()}
    //     member private this._Min () = List.min <| List.map (fun seg -> (seg.value :> IFloatValue<'p>).Min ()) this.segments
    //     member private this._Max () = List.max <| List.map (fun seg -> (seg.value :> IFloatValue<'p>).Max ()) this.segments
    //     member private this._Total pweight = pweight * List.sumBy (fun seg -> (seg.value :> IFloatValue<'p>).Total 1.0) this.segments
    //     interface IIntervalValue<'p,float> with
    //         member this.At pn p : float = 
    //             let ptopt = Sequence.getPointInSequence (this._AsSequence ()) pn p
    //             let pt = Option.defaultValue (failwith "TODO figure out discontinuity") ptopt
    //             pt.value
    //         member this.Split pn p self = 
    //             if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
    //             let before = Sequence.sliceByInterval pn {IntervalSlice.start=None;endbound=Some { position=p;strategy=SliceStrategy.Interpolated}} <| this._AsSequence ()
    //             let after = Sequence.sliceByInterval pn {IntervalSlice.start=Some {position=p;strategy=SliceStrategy.Interpolated};endbound=None}  <| this._AsSequence ()
    //             (asi {this with segments=before.intvalues},asi {this with segments=after.intvalues})
                
    //     interface IFloatValue<'p> with
    //         member this.Total pweight = this._Total pweight
    //         member this.Min () = this._Min ()
    //         member this.Max () = this._Max ()
    //         member this.Mean () = failwith "TODO get weighted mean" //List.averageBy (fun seg -> (seg.value :> IFloatValue<'p>).Mean ()) this.segments
    //         member this.Range () = this._Max () - this._Min ()


    let Aggregate (np:FloatValuedIntervalsNormalizer<'p>) op (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        let norms = np 1.0 inseq.intvalues
        let integs = Seq.map (fun nint -> op nint) norms
        let runningpairs = Seq.scan (+) 0.0 integs |> Seq.pairwise
        List.ofSeq <| Seq.map2 (fun i (sv,ev) -> LinearFloatInterval ({position=i.startbound;value=sv},{position=i.endbound;value=ev})) inseq.intvalues runningpairs

    let Total (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        Aggregate np (fun nint -> nint.interval.value.Total nint.weight) inseq

    let Mean (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        Aggregate np (fun nint -> nint.interval.value.Mean ()) inseq

    let Maximum (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        Aggregate np (fun nint -> nint.interval.value.Max ()) inseq

    let Minimum (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        Aggregate np (fun nint -> nint.interval.value.Min ()) inseq

    let Range (np:FloatValuedIntervalsNormalizer<'p>) (inseq:FloatValuedSequence<'p>) : FloatValuedInterval<'p> list =
        Aggregate np (fun nint -> nint.interval.value.Range ()) inseq