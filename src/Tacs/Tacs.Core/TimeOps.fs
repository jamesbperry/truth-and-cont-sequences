namespace Tacs.Core

module TimeOps =

    open System
    open Types

    let TimePosition (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let OnTimePosition (unitsize:float) (ints:Interval<DateTimeOffset,'v,'i> list) =
        let weight (int:Interval<DateTimeOffset,'v,'i>) u = float (int.endbound.position.Ticks - int.startbound.position.Ticks) / u
        List.map (fun (int:Interval<DateTimeOffset,'v,'i>) -> { interval=int;weight=weight int unitsize}) ints

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

    type ITimeValue<'p> =
        inherit IIntervalValue<'p,DateTimeOffset>
        // abstract member Min: unit -> DateTimeOffset //TODO
        // abstract member Max: unit -> DateTimeOffset
        // abstract member Mean: unit -> DateTimeOffset
        // abstract member Range: unit -> TimeSpan

    type LinearTimeValue<'p> = 
        {pstart:PointValue<'p,DateTimeOffset>;pend:PointValue<'p,DateTimeOffset>} with
        interface IIntervalValue<'p,DateTimeOffset> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,DateTimeOffset>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface ITimeValue<'p>
        end                    

    let LinearTimeValue (pstart,pend) =
        {LinearTimeValue.pstart=pstart;pend=pend} :> ITimeValue<'p>

    let LinearTimeInterval (startb:BoundaryValue<'p,DateTimeOffset>,endb:BoundaryValue<'p,DateTimeOffset>) =
        {startbound=startb.position;endbound=endb.position;value={LinearTimeValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb}}  //value :> ITimeValue<_> //TODO

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
                      