namespace Tacs.Core

module IntegerOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let IntegerPosition (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

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

    type LinearNearestIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearNearest pn (this.pstart, this.pend) p
            member this.Split pn p _ =
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
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearFloor pn (this.pstart, this.pend) p
            member this.Split pn p _ =
                let vmid = (this :> IIntervalValue<'p,int>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})

    type LinearCeilingIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearCeiling pn (this.pstart, this.pend) p
            member this.Split pn p _ =
                let vmid = (this :> IIntervalValue<'p,int>).At pn p
                let pmid = {position=p;value=vmid}
                failwith "not implemented"
                //({this with pend=pmid} :> IIntervalValue<'p,int>,{this with pstart=pmid} :> IIntervalValue<'p,int>)

    let LinearNearestIntValue (pstart,pend) =
        {LinearNearestIntValue.pstart=pstart;pend=pend}   

    let LinearFloorIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend}    

    let LinearCeilingIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend}

    let LinearNearestIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearNearestIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}}

    let LinearFloorIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearFloorIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}}
  

    let LinearCeilingIntInterval (startb:BoundaryValue<'p,int>,endb:BoundaryValue<'p,int>) =
        {startbound=startb.position;endbound=endb.position;value={LinearCeilingIntValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}}
                
    //Integer
    // let Integral (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"
    
    // let Average (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"

    // let Maximum (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"

    // let Minimum (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"

    // let Stdev (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"

    // let ValueRange (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
    //     failwith "not implemented"
                      