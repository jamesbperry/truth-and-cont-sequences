namespace Tacs.Core

module IntegerOps =

    open System
    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let IntegerPosition (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let OnIntPosition unitsize (ints:Interval<int,'v,'i> list) =
        List.map (fun int -> { interval=int;weight=float (int.endbound.position - int.startbound.position) / unitsize}) ints

    let InterpolatePosition (iv:Interval<int,'v,'i>) p : float =
        IntegerPosition iv.startbound.position iv.endbound.position p

    let InterpolateValueConstant (v:int) (p:'p) : int =
        v

    let InterpolateValueLinearNearest<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int (float dv * npos)
        pti.value + f

    let InterpolateValueLinearFloor<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) = 
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int <| floor (float dv * npos)
        pti.value + f

    let InterpolateValueLinearCeiling<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) = 
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int <| ceil (float dv * npos)
        pti.value + f

    type IIntegerValue<'p> =
        inherit IIntervalValue<'p,int>
        abstract member Integral: float -> float
        abstract member Min: unit -> int
        abstract member Max: unit -> int
        abstract member Mean: unit -> float
        abstract member Range: unit -> int

    type IntValuedInterval<'p> = Interval<'p,int,IIntegerValue<'p>>
    type IntValuedIntervalsNormalizer<'p> = float->IntValuedInterval<'p> list->NormalizedInterval<'p,int,IIntegerValue<'p>> list
    type IntValuedSequence<'p> = IntervalSequence<'p,int,IIntegerValue<'p>>

    type LinearNearestIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearNearest pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,int>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IIntegerValue<'p> with
            member this.Integral pweight = pweight * List.average [float this.pstart.value;float this.pend.value]
            member this.Min () = min this.pstart.value this.pend.value
            member this.Max () = max this.pstart.value this.pend.value
            member this.Mean () = List.average [float this.pstart.value;float this.pend.value]
            member this.Range () = abs <| this.pstart.value - this.pend.value            

    type LinearFloorIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>}
        member private this._Mean () = -0.5 * float (abs <| sign (this.pstart.value - this.pend.value)) + List.average [float this.pstart.value;float this.pend.value]
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearFloor pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,int>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IIntegerValue<'p> with
            member this.Integral pweight = pweight * this._Mean ()
            member this.Min () = min this.pstart.value this.pend.value
            member this.Max () = max this.pstart.value this.pend.value
            member this.Mean () = this._Mean ()
            member this.Range () = abs <| this.pstart.value - this.pend.value               

    type LinearCeilingIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>}
        member private this._Mean () = 0.5 * float (abs <| sign (this.pend.value - this.pstart.value)) + List.average [float this.pstart.value;float this.pend.value]
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearCeiling pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,int>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IIntegerValue<'p> with
            member this.Integral pweight = pweight * this._Mean ()
            member this.Min () = min this.pstart.value this.pend.value
            member this.Max () = max this.pstart.value this.pend.value
            member this.Mean () = this._Mean ()
            member this.Range () = abs <| this.pstart.value - this.pend.value    

    let LinearNearestIntValue (pstart,pend) =
        {LinearNearestIntValue.pstart=pstart;pend=pend} :> IIntegerValue<'p>  

    let LinearFloorIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend} :> IIntegerValue<'p>

    let LinearCeilingIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend} :> IIntegerValue<'p>

    let LinearNearestIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearNearestIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb} :> IIntegerValue<_>}

    let LinearFloorIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearFloorIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}:> IIntegerValue<_>} 
  

    let LinearCeilingIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearCeilingIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}:> IIntegerValue<_> }
                
    //Integer
    let Integral (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : FloatOps.FloatValuedInterval<'p> list =
        let norms = np 1.0 inseq.intvalues
        let integs = Seq.map (fun nint -> (nint.interval.value :> IIntegerValue<'p>).Integral nint.weight) norms
        let runningpairs = Seq.scan (+) 0.0 integs |> Seq.pairwise
        List.ofSeq <| Seq.map2 (fun i (sv,ev) -> FloatOps.LinearFloatInterval ({position=i.startbound;value=sv},{position=i.endbound;value=ev})) inseq.intvalues runningpairs
    
    let Average (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : IntValuedInterval<'p> list =
        failwith "not implemented"

    let Maximum (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : IntValuedInterval<'p> list =
        failwith "not implemented"

    let Minimum (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : IntValuedInterval<'p> list =
        failwith "not implemented"

    let Stdev (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : IntValuedInterval<'p> list =
        failwith "not implemented"

    let ValueRange (np:IntValuedIntervalsNormalizer<'p>) (inseq:IntValuedSequence<'p>) : IntValuedInterval<'p> list =
        failwith "not implemented"
                      