namespace Tacs.Core

module TimeOps =

    open System
    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let TimePosition (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let InterpolatePosition (iv:Interval<DateTimeOffset,'v,_>) p : float =
        TimePosition iv.startbound.position iv.endbound.position p

    let InterpolateValueConstant (v:DateTimeOffset) (p:'p)  =
        v
        
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,DateTimeOffset>*PointValue<'p,DateTimeOffset>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value.Subtract pti.value
        let npos = pinterp pti.position ptf.position p
        let del = TimeSpan.FromTicks <| int64 (float dv.Ticks * npos)
        pti.value + del

    type LinearTimeValue<'p> = 
        {pstart:PointValue<'p,DateTimeOffset>;pend:PointValue<'p,DateTimeOffset>} with
        interface IIntervalValue<'p,DateTimeOffset> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,DateTimeOffset>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})

    let LinearTimeValue (pstart,pend) =
        {LinearTimeValue.pstart=pstart;pend=pend}

    let LinearTimeInterval (startb:BoundaryValue<'p,DateTimeOffset>,endb:BoundaryValue<'p,DateTimeOffset>) =
        {startbound=startb.position;endbound=endb.position;value={LinearTimeValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}} 

    //DateTimeOffset
    // let Integrate (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"
    
    // let Average (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"

    // let Maximum (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"

    // let Minimum (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"

    // let Stdev (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"

    // let ValueRange (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
    //     failwith "not implemented"
                      