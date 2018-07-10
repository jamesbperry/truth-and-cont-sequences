namespace Tacs.Core

module Sequence =

    open Types

    type PointSequence<'p, 'v> = { id:string; ptvalues:PointValue<'p,'v> list; extrap:ExtrapolationStrategy; bound:BoundaryStrategy}

    type IntervalSequence<'p, 'v> = { id:string; intvalues:Interval<'p,'v> list }

    let instantaneousInterval (pos:'p) (v:'v) =
        InstantaneousInterval {instant=pos;value=ConstantValue v} //TODO clamp value to normalized
    
    let pointPairToInterval<'p,'v> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->IIntervalValue<'p,'v>) (bound:BoundaryStrategy) ptpair : Interval<'p,'v> =
            let (sp:PointValue<'p, 'v>, ep:PointValue<'p,'v>) = ptpair
            let iv = interp ptpair
            match bound with
            | InclusiveLow -> FiniteInterval {start=Inclusive sp.position;``end``=Exclusive ep.position; value=iv}
            | InclusiveHigh -> FiniteInterval {start=Exclusive sp.position;``end``=Inclusive ep.position; value=iv}

    let isBeforeBoundary (b:IntervalBoundary<'p>) (p:'p) =
        match b with
        | Inclusive i -> p < i
        | Exclusive e -> p <= e

    let isAfterBoundary (b:IntervalBoundary<'p>) (p:'p) = 
        match b with
        | Inclusive i -> p > i
        | Exclusive e -> p >= e

    let invertBound (b:IntervalBoundary<'p>) =
        match b with
        | Inclusive i -> Exclusive i
        | Exclusive e -> Inclusive e

    // let forwardRayConstantValueFunc<'p,'v when 'p : comparison> (pstart:'p) (v:'v) (p:'p) : ConstantValue<'p,'v> =
    //     if p >= pstart then ConstantValue v else failwith "Undefined"

    // let backwardRayConstantValueFunc<'p,'v when 'p : comparison> (pend:'p when 'p : comparison) (v:'v) (p:'p) : ConstantValue<'p,'v> =
    //     if p <= pend then ConstantValue v else failwith "Undefined"

    let extrapolateTo pn (extrap:ExtrapolationStrategy) (int:Interval<'p,'v>) =
        match extrap with
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | FiniteInterval fiv -> 
                let v = fiv.value.At pn fiv.start.position
                let rayend = invertBound fiv.start            
                //let rayval = backwardRayConstantValueFunc fiv.start.position v //TODO reimplement clamping of ray values to ray domain
                Some (BackwardRayInterval {``end``=rayend;value=ConstantValue v})
            | InstantaneousInterval iiv -> 
                let v = iiv.value.At pn iiv.instant
                let rayend = Exclusive iiv.instant
                //let rayval = backwardRayConstantValueFunc iiv.instant <| iiv.value iiv.instant
                Some (BackwardRayInterval {``end``=rayend;value=ConstantValue v})
            | _ -> None //TODO: handling/sanitizing a sequence with no nonzero-length intervals

        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let extrapolateFrom pn (extrap:ExtrapolationStrategy) (int:Interval<'p,'v>) =
        match extrap with
        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | FiniteInterval fiv -> 
                let v = fiv.value.At pn fiv.``end``.position
                let raystart = invertBound fiv.``end``
                //let rayval = forwardRayConstantValueFunc fiv.``end``.position v
                Some (ForwardRayInterval {start=raystart;value=ConstantValue v}) //TODO reimplement clamping of ray values to ray domain
            | InstantaneousInterval iiv -> 
                let v = iiv.value.At pn iiv.instant
                let raystart = Inclusive iiv.instant
                //let rayval = forwardRayConstantValueFunc iiv.instant <| iiv.value iiv.instant
                Some (ForwardRayInterval {start=raystart;value=ConstantValue v})
            | _ -> None
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let applyExtrapolation pn (extrap:ExtrapolationStrategy) (intseq:Interval<'p,'v> list) : Interval<'p,'v> list  =
        let head = List.head intseq
        let bef = extrapolateTo pn extrap head
        let beffolder s ray = 
            match s with
            | InstantaneousInterval _ :: t -> ray :: t
            | _ -> ray :: s
        let withbef = bef |> Option.fold beffolder intseq
        let aftfolder s ray =
            let last = Seq.tryLast withbef
            match last with
            | Some (InstantaneousInterval _) -> 
                let trunc = List.truncate (List.length s - 1) s //remove last. cringe.
                List.append trunc [ray]
            | _ -> List.append s [ray]
        let aft = extrapolateFrom pn extrap <| List.last intseq //ouch
        aft |> Option.fold aftfolder withbef |> List.ofSeq

    let remodelPairwiseToIntervals<'p,'v> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->IIntervalValue<'p,'v>) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v> =
        let ptPairs = Seq.pairwise ptseq.ptvalues 
        let ptsToInterval ptpair = pointPairToInterval interp ptseq.bound ptpair
        let lastPt = List.last ptseq.ptvalues
        let lastInt = instantaneousInterval lastPt.position <| lastPt.value
        let intervals = Seq.map ptsToInterval ptPairs |> Seq.append <| [lastInt] |> List.ofSeq
        {id=ptseq.id; intvalues=intervals}

    let remodelToLinearIntervals pn (interp:PointValue<'p,'v>*PointValue<'p,'v>->IIntervalValue<'p,'v>) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v>  =
        let ints = remodelPairwiseToIntervals interp ptseq
        let intsextrap = applyExtrapolation pn ptseq.extrap ints.intvalues 
        { ints with intvalues = intsextrap }

    let remodelToConstantIntervals pn (interp:PointValue<'p,'v>->IIntervalValue<'p,'v>) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v> = //TODO exorcise the 'v as obj mess
        let interpwrap (boundvals:PointValue<'p,'v> * PointValue<'p,'v>) =
            let (s, _) = boundvals
            interp s
        let ints = remodelPairwiseToIntervals interpwrap ptseq
        let intsextrap = applyExtrapolation pn ptseq.extrap ints.intvalues 
        { ints with intvalues = intsextrap }

    let remodelToPoints pn (ancType:RemodelAnchor) (bound:BoundaryStrategy) (inseq:IntervalSequence<'a,'b>) : PointSequence<'a,'b> =

        let pointMapper (anct:RemodelAnchor) (intv:Interval<'a,'b>) : PointValue<'a,'b> option =
            match intv with
            | FiniteInterval fiv ->
                match anct with
                | IntervalStart -> Some {position=fiv.start.position;value=(fiv.value.At pn fiv.start.position)}
                | IntervalEnd -> Some {position=fiv.``end``.position;value=fiv.value.At pn fiv.``end``.position}
            | ForwardRayInterval friv ->
                match anct with
                | IntervalStart -> Some {position=friv.start.position;value=friv.value.At pn friv.start.position}
                | _ -> None
            | BackwardRayInterval briv ->
                match anct with
                | IntervalEnd -> Some {position=briv.``end``.position;value=briv.value.At pn briv.``end``.position}
                | _ -> None
            | InstantaneousInterval iiv -> Some {position=iiv.instant;value=iiv.value.At pn iiv.instant}                     

        let hasfray =  Seq.exists (fun i -> match i with | ForwardRayInterval _ -> true; | _ -> false) inseq.intvalues
        let hasbray = Seq.exists (fun i -> match i with | BackwardRayInterval _ -> true; | _ -> false) inseq.intvalues
        let extrap = 
            match (hasfray, hasbray) with
            | (true, true) -> ExtrapolationStrategy.BeforeAndAfter
            | (true, false) -> ExtrapolationStrategy.BeforeFirst
            | (false, true) -> ExtrapolationStrategy.AfterLast
            | _ -> ExtrapolationStrategy.NoExtrapolation

        let ptvals = List.choose id <| List.map (pointMapper ancType) inseq.intvalues
        {id=inseq.id;extrap=extrap;ptvalues=ptvals;bound=bound}    

    let intervalEndsBefore (pos:'a) (intv:Interval<'a,'b>) =
        match intv with
            | FiniteInterval fiv -> pos |> isAfterBoundary fiv.``end``
            | BackwardRayInterval briv -> pos  |> isAfterBoundary briv.``end``
            | InstantaneousInterval iiv -> pos |> isAfterBoundary (Inclusive iiv.instant)
            | ForwardRayInterval _ -> false

    let intervalStartsAfter (pos:'a) (intv:Interval<'a,'b>) =
        match intv with
            | FiniteInterval fiv -> pos |> isBeforeBoundary fiv.start
            | BackwardRayInterval _ -> false
            | ForwardRayInterval friv -> pos |> isBeforeBoundary friv.start
            | InstantaneousInterval iiv -> pos |> isBeforeBoundary (Inclusive iiv.instant)

    let intervalContains (pos:'a) (intv:Interval<'a,'b>)  =
        let notcontains = (intv |> intervalEndsBefore pos) && (intv |> intervalStartsAfter pos)
        not notcontains //I'm not not licking toads

    let intervalOptContains (pos:'a) (intop:Interval<'a,'b> option) =
            match intop with
            | Some int -> intervalContains pos int
            | None -> false

    let getIntervalAtOrBefore (vals:Interval<'a,'b> list) (pos:'a) =
        let rec atOrBefore pos rem (prev:Interval<'a,'b> option) = 
            match rem with
            | [] -> None
            | (FiniteInterval h) :: t -> if intervalStartsAfter pos (FiniteInterval h) then prev else atOrBefore pos t (Some (FiniteInterval h))
            | (BackwardRayInterval h) :: t -> atOrBefore pos t (Some (BackwardRayInterval h))
            | [(ForwardRayInterval h)] -> if intervalStartsAfter pos (ForwardRayInterval h)  then None else (Some (ForwardRayInterval h))
            | (InstantaneousInterval iiv) :: t -> if intervalStartsAfter pos (InstantaneousInterval iiv) then prev else atOrBefore pos t (Some (InstantaneousInterval iiv))
            | _ :: t -> atOrBefore pos t prev
        atOrBefore pos vals None        

    let getNearestPointInInterval (intv:Interval<'a,'b> option) pn (pos:'a) : PointValue<'a,'b> option = //TODO this should clamp (?). Also, remove redundacy.
        match intv with //Hmmm... this implies a defined behavior for values interpolated outside bounds. No bueno.
        | Some (FiniteInterval fiv) -> Some {position=pos;value=fiv.value.At pn pos}
        | Some (InstantaneousInterval iiv) -> Some {position=pos;value=iiv.value.At pn pos}
        | Some (ForwardRayInterval friv) -> Some {position=pos;value=friv.value.At pn pos}
        | Some (BackwardRayInterval briv) -> Some {position=pos;value=briv.value.At pn pos}
        | None -> None
    
    let getExactPointInInterval (intop:Interval<'a,'b> option) pn (pos:'a) : PointValue<'a,'b> option =
        let cont = intervalOptContains pos intop
        match cont with
        | true -> getNearestPointInInterval intop pn pos
        | false -> None

    let getPointInSequence (inseq:IntervalSequence<'a,'b>) pn (pos:'a) =
        let prev = getIntervalAtOrBefore inseq.intvalues pos
        getExactPointInInterval prev pn pos

    let SomeNonzeroLength (int:Interval<'a,'b>) =
        let isNonzeroLength (fiv:FiniteInterval<'a,'b>) = fiv.start <> fiv.``end``
        match int with
        | FiniteInterval fiv -> if (isNonzeroLength fiv) then Some int else None
        | _ -> Some int //not finite so cannot be zero length

    let splitInterval (intop:Interval<'a,'b> option) (bound:BoundaryStrategy) pn (pos:'a) =
        let endpto = getNearestPointInInterval intop pn pos
        let trim int bnd (mid:PointValue<'a,'b>) =  
            let (befmid, aftmid) =
                match bnd with
                | InclusiveLow -> (Exclusive mid.position, Inclusive mid.position)
                | InclusiveHigh -> (Inclusive mid.position, Exclusive mid.position)
            match int with
            | BackwardRayInterval briv -> 
                {before=Some <| BackwardRayInterval {``end``=befmid;value=briv.value};after=SomeNonzeroLength <| FiniteInterval {start=aftmid;``end``=briv.``end``;value=briv.value}}
            | FiniteInterval fiv -> 
                {before=SomeNonzeroLength <| FiniteInterval {start=fiv.start;``end``=befmid;value=fiv.value};after=SomeNonzeroLength <| FiniteInterval {start=aftmid;``end``=fiv.``end``;value=fiv.value}}
            | ForwardRayInterval friv -> 
                {before=SomeNonzeroLength <| FiniteInterval {start=friv.start;``end``=befmid;value=friv.value};after=Some <| ForwardRayInterval {start=aftmid;value=friv.value}}
            | InstantaneousInterval _ -> {before=Some int;after=Some int}

        let trimopt = Option.map3 trim
        Option.defaultValue {before=None;after=None} <| trimopt intop (Some bound) endpto     

    let trimIntervalTo (intop:Interval<'a,'b> option) pn (pos:IntervalBoundary<'a>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveHigh
            | Exclusive _ -> BoundaryStrategy.InclusiveLow
        let split = splitInterval intop strat pn pos.position
        split.before

    let trimIntervalFrom (intop:Interval<'a,'b> option) pn (pos:IntervalBoundary<'a>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveLow
            | Exclusive _ -> BoundaryStrategy.InclusiveHigh
        let split = splitInterval intop strat pn pos.position
        split.after

    let getIntervalsAtAndAfter  (inseq:IntervalSequence<'a,'b>) (strat:SliceStrategy) pn (pos:'a) =
        let skipper (intval:Interval<'a,'b>) = intervalEndsBefore pos intval
        let skipped = Seq.skipWhile skipper inseq.intvalues
        let bounded =
             match strat with
                | Inside -> Seq.skip 1 skipped
                | Intersected -> skipped
                | Interpolated -> 
                    let head = Seq.head skipped
                    match (intervalContains pos <| head) with
                    | true -> 
                        let shead = Some head
                        let split = splitInterval shead InclusiveLow pn pos
                        match (split.after) with
                        | Some sint -> Seq.append [sint] skipped
                        | None -> skipped
                    | false -> skipped                
        List.ofSeq bounded

    let getIntervalsAtAndBefore (inseq:IntervalSequence<'a,'b>) (strat:SliceStrategy) pn (pos:'a) = 
        match strat with
            | Inside -> List.takeWhile (fun v -> intervalEndsBefore pos v) inseq.intvalues
            | Intersected -> List.takeWhile(fun v -> not <| intervalStartsAfter pos v) inseq.intvalues
            | Interpolated -> 
                let wintersect = List.takeWhile(fun v -> not <| intervalStartsAfter pos v) inseq.intvalues
                let rwintersect = List.rev wintersect
                match rwintersect with
                | (h::t) ->
                    match (intervalContains pos h) with
                    | true -> 
                        let split = splitInterval (Some h) InclusiveHigh pn pos
                        match (split.after) with
                        | Some sint -> sint :: t
                        | None -> t
                    | false -> t     
                | [] -> []                           


    let sliceByInterval pn (s:IntervalSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = 
            match s.start with
            | Some start -> getIntervalsAtAndAfter inseq start.strategy pn start.position
            | None -> inseq.intvalues  
        let etrim =
            match s.``end`` with
            | Some e -> getIntervalsAtAndBefore {inseq with intvalues=strim} e.strategy pn e.position
            | None -> strim
        {inseq with intvalues=etrim}

    let sliceForwardByCount pn (s:ForwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = getIntervalsAtAndAfter inseq s.start.strategy pn s.start.position
        let ctrim = List.truncate s.count strim
        {inseq with intvalues=ctrim}

    let takeLast n seq =  //cringe
        let rev = Seq.rev seq
        let trim = Seq.take n rev
        Seq.rev trim |> List.ofSeq

    let sliceBackwardByCount pn (s:BackwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = getIntervalsAtAndBefore inseq s.``end``.strategy pn s.``end``.position
        let ctrim = takeLast s.count strim
        {inseq with intvalues=ctrim}

    let windowHopping (h:HoppingWindowing<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b> list) = 
        failwith "not implemented"

    let windowSliding (s:IntervalSize<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b> list) = 
        failwith "not implemented"

    let window (w:Windowing<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b> list) =    
        match w with
        | Single _ -> [inseq]
        | Sliding s -> windowSliding s inseq
        | Hopping h -> windowHopping h inseq

    // let length (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
    //     let noextrap = removeExtrapolation inseq.intvalues
    //     let lenval = match noextrap with
    //     | [] -> []
    //     | h :: t -> //TODO pull out
    //         let first = List.head noextrap
    //         let last = List.last noextrap
    //         let sbound = first.``start``.position
    //         let ebound = last.``end``
    //         let diff = ebound - sbound
    //         [FiniteIntervalValue {start={position=sbound;value=diff};``end``={position=ebound;value=diff}}]
    //     {id=inseq.id;extrap=ExtrapolationStrategy.NoExtrapolation;interp=TODO;intvalues=lenval}  

    // let aggregateAll (op:AggregationOperation) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
    //     match op with
    //     | NoOp -> inseq
    //     | Custom c -> failwith "not implemented"
    //     | Integral -> integrate inseq
    //     | Avg -> average inseq
    //     | Max -> maximum inseq
    //     | Min -> minimum inseq
    //     | Std -> stdev inseq
    //     | Range -> valuerange inseq

    // let aggregate (a:Aggregate<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
    //     let wins = window a.windowing inseq
    //     let aggs = List.map (fun win -> aggregateAll a.operation win) wins
    //     let vals = List.collect (fun s -> s.intvalues) aggs
    //     {inseq with intvalues=vals}

