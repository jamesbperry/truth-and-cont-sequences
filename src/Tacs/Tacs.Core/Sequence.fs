namespace Tacs.Core

module Sequence =

    open Types

    type PointSequence<'p, 'v> = { id:string; ptvalues:PointValue<'p,'v> list; extrap:ExtrapolationStrategy; bound:BoundaryStrategy}

    type IntervalSequence<'p, 'v, 'i when 'i :> IIntervalValue<'p,'v>> = { id:string; intvalues:Interval<'p,'v,'i> list; preextrap:IExtrapolation<'p,'v>; postextrap:IExtrapolation<'p,'v> }

    let ConstantInterval<'p,'v,'i> sbound ebound v = {startbound=sbound;endbound=ebound;value=ConstantValue v}

    let Instant (pos:'p) (v:'v) = {startbound=Inclusive pos;endbound=Inclusive pos;value=ConstantValue v} //TODO clamp value to normalized
    
    let pointPairToInterval<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->'i) (startIncl) (endIncl) ptpair : Interval<'p,'v,'i> =
            let (sp:PointValue<'p, 'v>, ep:PointValue<'p,'v>) = ptpair
            let iv = interp ptpair
            let sb = 
                match startIncl with
                | IsInclusive -> Inclusive sp.position
                | IsExclusive -> Exclusive sp.position
            let eb = 
                match endIncl with
                | IsInclusive -> Inclusive ep.position
                | IsExclusive -> Exclusive ep.position
            {startbound=sb;endbound=eb; value=iv}

    let inline isBeforeBoundary (b:IntervalBoundary<'p>) (p:'p) =
        match b with
        | Inclusive i -> p < i
        | Exclusive e -> p <= e

    let inline isAfterBoundary (b:IntervalBoundary<'p>) (p:'p) = 
        match b with
        | Inclusive i -> p > i
        | Exclusive e -> p >= e

    let invertBound (b:IntervalBoundary<'p>) =
        match b with
        | Inclusive i -> Exclusive i
        | Exclusive e -> Inclusive e

    type NoExtrapolation<'p,'v> =
        new() = {}
        interface IExtrapolation<'p,'v> with
            member _this.At pn _ =
                None         

    type ConstantBefore<'p,'v when 'p : comparison> =
        { endbound:IntervalBoundary<'p>; constantextrap:'v }
        interface IExtrapolation<'p,'v> with
            member this.At pn p =
                match p |> isBeforeBoundary this.endbound with
                | true -> Some this.constantextrap
                | false -> None    
    let ConstantBefore b v = { endbound=b; constantextrap=v } :> IExtrapolation<_,_>
    let ConstantBeforeInclusive p v = { endbound=Inclusive p; constantextrap=v } :> IExtrapolation<_,_>
    let ConstantBeforeExclusive p v = { endbound=Exclusive p; constantextrap=v } :> IExtrapolation<_,_>

    type ConstantAfter<'p,'v when 'p : comparison> =
        { startbound:IntervalBoundary<'p>; constantextrap:'v }
        interface IExtrapolation<'p,'v> with
            member this.At pn p =
                match p |> isAfterBoundary this.startbound with
                | true -> Some this.constantextrap
                | false -> None        
    let ConstantAfter b v = { startbound=b; constantextrap=v } :> IExtrapolation<_,_>
    let ConstantAfterInclusive p v = { startbound=Inclusive p; constantextrap=v } :> IExtrapolation<_,_>
    let ConstantAfterExclusive p v = { startbound=Exclusive p; constantextrap=v } :> IExtrapolation<_,_>

    let withDifferentLast (coll:'T list) (last:'T) =
        let n = max 0 <| coll.Length - 1
        let trunc = Seq.take n coll
        List.ofSeq <| Seq.append trunc [last]

    let remodelPairwiseToIntervals<'p,'v, 'i when 'p : comparison and 'i :> IIntervalValue<'p,'v>> pn (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->'i) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v,'i> =
        let combineStrats (b1:BoundaryStrategy) (b2:BoundaryStrategy) =
            match (b1,b2) with
            | (InclusiveHigh,InclusiveHigh) -> (IsExclusive,IsInclusive)
            | (InclusiveLow,InclusiveLow) -> (IsInclusive,IsExclusive)
            | _ -> (IsInclusive,IsInclusive)    
        let getStrats (b:BoundaryStrategy) =
            match b with
            | InclusiveHigh -> (IsExclusive,IsInclusive)
            | InclusiveLow -> (IsInclusive,IsExclusive)        
        let rec ptListToInterval seqbstrat pts ints =
            match (pts,ints) with
            | ([],_) -> []
            | ([_],_) -> [] //1 or fewer points remaining, no op
            | (first :: sec :: t,[]) -> 
                let (bss,bse) = combineStrats seqbstrat InclusiveLow //inclusive start at least
                let ni = pointPairToInterval interp bss bse (first,sec) 
                ptListToInterval seqbstrat (sec :: t) (ni :: ints)
            | ([penu;last],_) ->
                let (bss,bse) = combineStrats seqbstrat InclusiveHigh //inclusive end at least
                let ni = pointPairToInterval interp bss bse (penu,last) 
                List.rev <| ni :: ints
            | (x :: y :: t,_) -> 
                let (bss,bse) = getStrats seqbstrat
                let ni = pointPairToInterval interp bss bse (x,y) //just regular type
                ptListToInterval seqbstrat (y :: t) (ni :: ints)

        let intervals = ptListToInterval ptseq.bound ptseq.ptvalues []

        let preextrap = //use default extrapolation if called for
            match ptseq.extrap with
            | BeforeFirst | BeforeAndAfter -> 
                let firstint = List.head intervals
                let v = firstint.value.At pn firstint.startbound.position
                {endbound=invertBound firstint.startbound;constantextrap=v} :> IExtrapolation<'p,'v>
            | _ -> NoExtrapolation<'p,'v>() :> IExtrapolation<'p,'v>
        let postextrap = //use default extrapolation if called for
            match ptseq.extrap with
            | AfterLast | BeforeAndAfter -> 
                let lastint = List.last intervals
                let v = lastint.value.At pn lastint.endbound.position
                {startbound=invertBound lastint.startbound;constantextrap=v} :> IExtrapolation<'p,'v>
            | _ -> NoExtrapolation<'p,'v>() :> IExtrapolation<'p,'v>
        {id=ptseq.id; intvalues=intervals;preextrap=preextrap;postextrap=postextrap}

    let remodelToPoints pn (bound:BoundaryStrategy) (inseq:IntervalSequence<'p,'v,_>) : PointSequence<'p,'v> =

        let ptvals = 
            seq { 
                for int in inseq.intvalues do
                    match int.startbound with
                    | Inclusive _ -> yield {position=int.startbound.position;value=(int.value.At pn int.startbound.position)}
                    | _ -> ()
                    match int.endbound with
                    | Inclusive _ -> yield {position=int.endbound.position;value=int.value.At pn int.endbound.position}
                    | _ -> ()
            } |> List.ofSeq        
        let hasfray = 
            match inseq.postextrap with
            | :? NoExtrapolation<'p,'v> -> false
            | _ -> true
        let hasbray = 
            match inseq.preextrap with
            | :? NoExtrapolation<'p,'v> -> false
            | _ -> true
        let extrap = 
            match (hasfray, hasbray) with
            | (true, true) -> ExtrapolationStrategy.BeforeAndAfter
            | (true, false) -> ExtrapolationStrategy.BeforeFirst
            | (false, true) -> ExtrapolationStrategy.AfterLast
            | _ -> ExtrapolationStrategy.NoExtrapolation

        {id=inseq.id;extrap=extrap;ptvalues=ptvals;bound=bound}    

    let intervalEndsBefore (pos:'p) (intv:Interval<'p,'v,'i>) =
        isAfterBoundary intv.endbound pos

    let intervalStartsAfter (pos:'p) (intv:Interval<'p,'v,'i>) =
        isBeforeBoundary intv.startbound pos

    let intervalContains (pos:'p) (intv:Interval<'p,'v,'i>)  =
        let notcontains = (intv |> intervalEndsBefore pos) || (intv |> intervalStartsAfter pos)
        not notcontains //I'm not not licking toads

    let intervalOptContains (pos:'p) (intop:Interval<'p,'v,'i> option) =
            match intop with
            | Some int -> intervalContains pos int
            | None -> false

    let getIntervalAtOrBefore (vals:Interval<'p,'v,'i> list) (pos:'p) =
        let rec atOrBefore pos rem (prev:Interval<'p,'v,'i> option) = 
            match rem with
            | [] -> None
            | h :: t -> if intervalStartsAfter pos h then prev else atOrBefore pos t (Some h)
        atOrBefore pos vals None        

    let getNearestPointInInterval (intopt:Interval<'p,'v,'i> option) pn (pos:'p) : PointValue<'p,'v> option = //TODO this should clamp (?). Also, remove redundacy.
        match intopt with //Hmmm... this implies a defined behavior for values interpolated outside bounds. No bueno.
        | Some int -> Some {position=pos;value=int.value.At pn pos}
        | None -> None
    
    let getExactPointInInterval (intop:Interval<'p,'v,'i> option) pn (pos:'p) : PointValue<'p,'v> option =
        let cont = intervalOptContains pos intop
        match cont with
        | true -> getNearestPointInInterval intop pn pos
        | false -> None

    let getPointInSequence (inseq:IntervalSequence<'p,'v,'i>) pn (pos:'p) = //TODO account for extrapolation on sequence
        let prev = getIntervalAtOrBefore inseq.intvalues pos
        getExactPointInInterval prev pn pos

    let SomeNonzeroLength (int:Interval<'p,'v,'i>) =
        match int.startbound.position < int.endbound.position with
        | true -> Some int
        | false -> None

    let splitInterval (intop:Interval<'p,'v,'i> option) (bound:BoundaryStrategy) pn (pos:'p) =
        let trim (int:Interval<'p,'v,'i>) bnd = //todo clamp pos into interval 
            let (vbef, vaft) = int.value.Split pn pos int.value
            let (pbef, paft) =
                match bnd with
                | InclusiveLow -> (Exclusive pos, Inclusive pos)
                | InclusiveHigh -> (Inclusive pos, Exclusive pos)

            {before=Some {startbound=int.startbound;endbound=pbef;value=vbef};after=Some {startbound=paft;endbound=int.endbound;value=vaft}}

        let trimopt = Option.map2 trim
        Option.defaultValue {before=None;after=None} <| trimopt intop (Some bound)  

    let trimIntervalTo (intop:Interval<'p,'v,'i> option) pn (pos:IntervalBoundary<'p>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveHigh
            | Exclusive _ -> BoundaryStrategy.InclusiveLow
        let split = splitInterval intop strat pn pos.position
        split.before

    let trimIntervalFrom (intop:Interval<'p,'v,'i> option) pn (pos:IntervalBoundary<'p>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveLow
            | Exclusive _ -> BoundaryStrategy.InclusiveHigh
        let split = splitInterval intop strat pn pos.position
        split.after

    let getIntervalsAtAndAfter  (inseq:IntervalSequence<'p,'v,'i>) (strat:SliceStrategy) pn (pos:'p) =
        let skipper (intval:Interval<'p,'v,'i>) = intervalEndsBefore pos intval
        let skipped = Seq.skipWhile skipper inseq.intvalues
        let bounded =
             match strat with
                | Inside ->
                    let head = Seq.head skipped
                    if intervalStartsAfter pos head then skipped else Seq.skip 1 skipped
                | Intersected -> skipped
                | Interpolated -> 
                    let head = Seq.head skipped
                    let tail = Seq.tail skipped
                    match (intervalContains pos <| head) with
                    | true -> 
                        let shead = Some head
                        let split = splitInterval shead InclusiveLow pn pos
                        match (split.after) with
                        | Some sint -> Seq.append [sint] tail
                        | None -> skipped
                    | false -> skipped                
        List.ofSeq bounded

    let getIntervalsAtAndBefore (inseq:IntervalSequence<'p,'v,'i>) (strat:SliceStrategy) pn (pos:'p) = 
        match strat with
            | Inside -> List.takeWhile (fun v -> intervalEndsBefore pos v) inseq.intvalues
            | Intersected -> List.takeWhile(fun v -> not <| intervalStartsAfter pos v) inseq.intvalues
            | Interpolated -> 
                let wintersect = List.takeWhile(fun v -> not <| intervalStartsAfter pos v) inseq.intvalues
                let lastopt = List.tryLast wintersect
                match lastopt with
                | Some last ->
                    match (intervalContains pos last) with
                    | true -> 
                        let split = splitInterval lastopt InclusiveHigh pn pos
                        match (split.before) with
                        | Some sint -> withDifferentLast wintersect sint
                        | None -> wintersect
                    | false -> wintersect    
                | None -> []                
        
    let sliceByInterval pn (s:IntervalSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) = 
        let strim = 
            match s.start with
            | Some start -> getIntervalsAtAndAfter inseq start.strategy pn start.position
            | None -> inseq.intvalues  
        let etrim =
            match s.endbound with
            | Some e -> getIntervalsAtAndBefore {inseq with intvalues=strim} e.strategy pn e.position
            | None -> strim      
        {inseq with intvalues=etrim}

    let sliceForwardByCount pn (s:ForwardSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
        let strim = getIntervalsAtAndAfter inseq s.start.strategy pn s.start.position
        let ctrim = List.truncate s.count strim
        {inseq with intvalues=ctrim}

    let takeLast n list = 
        let len = List.length list
        let nskip = len - n |> max 0
        List.skip nskip list

    let sliceBackwardByCount pn (s:BackwardSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
        let strim = getIntervalsAtAndBefore inseq s.endbound.strategy pn s.endbound.position
        let ctrim = takeLast s.count strim
        {inseq with intvalues=ctrim}

    let windowHopping (h:HoppingWindowing<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) = 
        failwith "not implemented"

    let windowSliding (s:IntervalSize<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) = 
        failwith "not implemented"

    let window (w:Windowing<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) =    
        match w with
        | Single _ -> [inseq]
        | Sliding s -> windowSliding s inseq
        | Hopping h -> windowHopping h inseq

    // let length (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
    //     let noextrap = removeExtrapolation inseq.intvalues
    //     let lenval = match noextrap with
    //     | [] -> []
    //     | h :: t -> //TODO pull out
    //         let first = List.head noextrap
    //         let last = List.last noextrap
    //         let sbound = first.``start``.position
    //         let ebound = last.endbound
    //         let diff = ebound - sbound
    //         [FiniteIntervalValue {start={position=sbound;value=diff};endbound={position=ebound;value=diff}}]
    //     {id=inseq.id;extrap=ExtrapolationStrategy.NoExtrapolation;interp=TODO;intvalues=lenval}  

    // let aggregateAll (op:AggregationOperation) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
    //     match op with
    //     | NoOp -> inseq
    //     | Custom c -> failwith "not implemented"
    //     | Integral -> integrate inseq
    //     | Avg -> average inseq
    //     | Max -> maximum inseq
    //     | Min -> minimum inseq
    //     | Std -> stdev inseq
    //     | Range -> valuerange inseq

    // let aggregate (a:Aggregate<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
    //     let wins = window a.windowing inseq
    //     let aggs = List.map (fun win -> aggregateAll a.operation win) wins
    //     let vals = List.collect (fun s -> s.intvalues) aggs
    //     {inseq with intvalues=vals}

