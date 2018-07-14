namespace Tacs.Core

module Sequence =

    open Types
    let pointPairToInterval<'p,'v,'i when 'i :> IIntervalValue<'p,'v>> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->'i) (startIncl) (endIncl) ptpair : Interval<'p,'v,'i> =
            let (sp:PointValue<'p, 'v>, ep:PointValue<'p,'v>) = ptpair
            let iv = interp ptpair
            let sb = 
                match startIncl with
                | AsInclusive -> Inclusive sp.position
                | AsExclusive -> Exclusive sp.position
            let eb = 
                match endIncl with
                | AsInclusive -> Inclusive ep.position
                | AsExclusive -> Exclusive ep.position
            {startbound=sb;endbound=eb; value=iv}

    let inline isBeforeBoundary (b:IntervalBoundary<'p>) (p:'p) = p < b.position
    let inline isAfterBoundary (b:IntervalBoundary<'p>) (p:'p) = p > b.position

    let invertBound (b:IntervalBoundary<'p>) =
        match b with
        | Inclusive i -> Exclusive i
        | Exclusive e -> Inclusive e

    [<RequireQualifiedAccess>]
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
            | (InclusiveHigh,InclusiveHigh) -> (AsExclusive,AsInclusive)
            | (InclusiveLow,InclusiveLow) -> (AsInclusive,AsExclusive)
            | _ -> (AsInclusive,AsInclusive)    
        let getStrats (b:BoundaryStrategy) =
            match b with
            | InclusiveHigh -> (AsExclusive,AsInclusive)
            | InclusiveLow -> (AsInclusive,AsExclusive)        
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
            | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.BeforeAndAfter -> 
                let firstint = List.head intervals
                let v = firstint.value.At pn firstint.startbound.position
                {endbound=invertBound firstint.startbound;constantextrap=v} :> IExtrapolation<'p,'v>
            | _ -> NoExtrapolation<'p,'v>() :> IExtrapolation<'p,'v>
        let postextrap = //use default extrapolation if called for
            match ptseq.extrap with
            | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter -> 
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

    let intervalEndsBefore (b:IntervalBoundary<'p>) (intv:Interval<'p,'v,'i>) =
        match intv.endbound with
        | Inclusive _ -> 
            match b with
            | Inclusive bi -> isAfterBoundary intv.endbound bi
            | Exclusive be -> not <| isBeforeBoundary intv.endbound be
        | Exclusive _ -> not <| isBeforeBoundary intv.endbound b.position

    let intervalEndsAfter (b:IntervalBoundary<'p>) (intv:Interval<'p,'v,'i>) =
        intv.endbound.position > b.position

    let intervalStartsAfter (b:IntervalBoundary<'p>) (intv:Interval<'p,'v,'i>) =
        match intv.startbound with
        | Inclusive _ -> 
            match b with
            | Inclusive bi -> isBeforeBoundary intv.startbound bi
            | Exclusive be -> not <| isAfterBoundary intv.startbound be
        | Exclusive _ -> not <| isAfterBoundary intv.startbound b.position

    let intervalStartsBefore (b:IntervalBoundary<'p>) (intv:Interval<'p,'v,'i>) =
        intv.startbound.position < b.position

    //let intervalStartsAtOrAfter

    let intervalContains (pos:'p) (intv:Interval<'p,'v,'i>)  =
        let notcontains = (intv |> intervalEndsBefore (Inclusive pos)) || (intv |> intervalStartsAfter (Inclusive pos))
        not notcontains //I'm not not licking toads

    let intervalOptContains (pos:'p) (intop:Interval<'p,'v,'i> option) =
            match intop with
            | Some int -> intervalContains pos int
            | None -> false

    let getIntervalAtOrBefore (vals:Interval<'p,'v,'i> list) (pos:'p) =
        let rec atOrBefore pos rem (prev:Interval<'p,'v,'i> option) = 
            match rem with
            | [] -> None
            | h :: t -> if intervalStartsAfter (Inclusive pos) h then prev else atOrBefore pos t (Some h)
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

 
    let getIntervalAt (vals:Interval<'p,'v,'i> list) (pos:'p) =
        List.tryFind (fun int -> intervalContains pos int) vals

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
            let (befstart,aftend) =
                match (int.startbound =@= int.endbound) with
                | false -> (int.startbound,int.endbound)
                | true -> (Inclusive int.startbound.position,Inclusive int.endbound.position) //TODO disallow case where zero-length interval is exclusive

            {before=Some {startbound=befstart;endbound=pbef;value=vbef};after=Some {startbound=paft;endbound=aftend;value=vaft}}

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

    let getIntervalsAtAndAfter (inseq:IntervalSequence<'p,'v,'i>) (strat:SliceStrategy) pn b =
        let skipper (intval:Interval<'p,'v,'i>) = intervalEndsBefore b intval
        let skipped = Seq.skipWhile skipper inseq.intvalues
        let bounded =
             match strat with
                | Inside ->
                    let head = Seq.head skipped
                    if intervalStartsAfter b head then skipped else Seq.skip 1 skipped
                | Intersected -> skipped
                | Interpolated -> 
                    let head = Seq.head skipped
                    let tail = Seq.tail skipped
                    match (intervalContains b.position <| head) with
                    | true -> 
                        let hilo =
                            match b with
                            | Inclusive _ -> InclusiveLow // split as )[ or ](
                            | Exclusive _ -> InclusiveHigh
                        let shead = Some head
                        let split = splitInterval shead hilo pn b.position
                        match (split.after) with
                        | Some sint -> Seq.append [sint] tail
                        | None -> skipped
                    | false -> skipped                
        List.ofSeq bounded

    let getIntervalsAtAndBefore (inseq:IntervalSequence<'p,'v,'i>) (strat:SliceStrategy) pn b = 
        match strat with
            | Inside -> List.takeWhile (fun v -> intervalEndsBefore (b) v) inseq.intvalues
            | Intersected -> List.takeWhile (fun v -> not <| intervalStartsAfter (b) v) inseq.intvalues
            | Interpolated -> 
                let wintersect = List.takeWhile (fun v -> not <| intervalStartsAfter (b) v) inseq.intvalues
                let lastopt = List.tryLast wintersect
                match lastopt with
                | Some last ->
                    match (intervalContains b.position last) with
                    | true -> 
                        let hilo =
                            match b with
                            | Inclusive _ -> InclusiveHigh // split as )[ or ](
                            | Exclusive _ -> InclusiveLow
                        let split = splitInterval lastopt hilo pn b.position
                        match (split.before) with
                        | Some sint -> withDifferentLast wintersect sint
                        | None -> wintersect
                    | false -> wintersect    
                | None -> []                
        
    let sliceByInterval pn (s:IntervalSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) = 
        let strim = 
            match s.start with
            | Some start -> getIntervalsAtAndAfter inseq start.strategy pn start.boundary
            | None -> inseq.intvalues  
        let etrim =
            match s.endbound with
            | Some eb -> getIntervalsAtAndBefore {inseq with intvalues=strim} eb.strategy pn eb.boundary
            | None -> strim           
        {inseq with intvalues=etrim}

    let sliceByBoundariesInterpolated pn (startb,endb) inseq =
        let makeSlicer b = {boundary=b;strategy=Interpolated}
        let slicer = {start=Some <| makeSlicer startb;endbound=Some <|makeSlicer endb}
        sliceByInterval pn slicer inseq

    let sliceForwardByCount pn (s:ForwardSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
        let strim = getIntervalsAtAndAfter inseq s.start.strategy pn s.start.boundary
        let ctrim = List.truncate s.count strim
        {inseq with intvalues=ctrim}

    let takeLast n list = 
        let len = List.length list
        let nskip = len - n |> max 0
        List.skip nskip list

    let sliceBackwardByCount pn (s:BackwardSlice<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
        let strim = getIntervalsAtAndBefore inseq s.endbound.strategy pn s.endbound.boundary
        let ctrim = takeLast s.count strim
        {inseq with intvalues=ctrim}

    let bounds inseq : (IntervalBoundary<_>*IntervalBoundary<_>) =
        let firstint = List.head inseq.intvalues
        let lastint = List.last inseq.intvalues
        (firstint.startbound,lastint.endbound)

    let inline incrementBy (del:'dp) (pos:'p when 'p : (static member (+) : 'p * 'dp -> 'p)) =
        pos + del

    let inline decrementBy (del:'dp) (pos:'p when 'p : (static member (-) : 'p * 'dp -> 'p)) =
        pos - del

    let inline tumblingWindowDelBounds (a:Anchor<'p>) wd (del:'dp) (inseq:IntervalSequence<'p,_,_>)  : (IntervalBoundary<'p>*IntervalBoundary<'p>) list =
        let (seqstart,seqend) = bounds inseq
        let someIfStartsBefore (start:'p) (del:'dp) (before:'p) : ('p*'p) option =
            if start < before then Some (start, start |> incrementBy del) else None

        let getFrom (startpos:'p) del (endpos:'p) : ('p*'p) list =
            let tup e = Option.map (fun p -> (p,p)) <| someIfStartsBefore e del endpos
            List.unfold (fun (_,e) -> tup e) (startpos,startpos)
        
        let positions = getFrom seqstart.position del seqend.position

        let asWindow wd (s,e) =
            match wd with
            | LookingForward -> (Inclusive s, Exclusive e)
            | LookingBackward -> (Exclusive s, Inclusive e)

        positions |> List.map (asWindow wd)

    let inline windowTumbling pn (a:Anchor<'p>) (wd:WindowingDirection) (is:IntervalSize<'dp>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) =
        let dp = 
            match is with
            | Width w -> w
            | _ -> failwith "not implemented"
        let windows = tumblingWindowDelBounds a wd dp inseq
        windows |> List.map (fun w -> sliceByBoundariesInterpolated pn (w) inseq)

    let inline windowSliding pn a wd (is:IntervalSize<'dp>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) = 
        failwith "not implemented"

    let inline windowHopping pn a wd (h:HoppingWindowing<'dp>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) = 
        failwith "not implemented"

    let inline window pn (w:Windowing<'p,'dp>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i> list) =    
        match w with
        | Single _ -> [inseq]
        | Tumbling (a,wd,is) -> windowTumbling pn a wd is inseq   
        | Sliding (a,wd,is) -> windowSliding pn a wd is inseq
        | Hopping (a,wd,h) -> windowHopping pn a wd h inseq

    // let aggregate (a:Aggregate<'p>) (inseq:IntervalSequence<'p,'v,'i>) : (IntervalSequence<'p,'v,'i>) =
    //     let wins = window a.windowing inseq
    //     let aggs = List.map (fun win -> aggregateAll a.operation win) wins
    //     let vals = List.collect (fun s -> s.intvalues) aggs
    //     {inseq with intvalues=vals}

    let sample pn (samp:Sample<'p,'dp>) (inseq:IntervalSequence<'p,'v,'i>) : (PointValue<'p,'v> option list) =
        match samp with
        | SampleAt p -> [getPointInSequence inseq pn p] //Option.defaultValue [] <| Option.bind (fun p -> Some [p]) (getPointInSequence inseq pn p)
        | SamplesAt pl -> List.map (fun p -> getPointInSequence inseq pn p) pl //TODO do efficiently, this is O(mn)
        | SampleIntervals (size,anc) -> failwith "not implemented"

(*
    EOD thoughts:
    - windowing needs look directions, sliding/hopping, and byCount for tumbling
    - sampling needs intervals and tests for sample(s)at
    - TimeValue needs to be brought into the 21st century, i.e. implement data interfaces
*)    