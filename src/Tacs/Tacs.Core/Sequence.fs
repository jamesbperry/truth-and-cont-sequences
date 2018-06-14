namespace Tacs.Core

module Sequence =

    open Types

    type PointSequence<'p, 'v> = { id:string; ptvalues:PointValue<'p,'v> list; extrap:ExtrapolationStrategy; bound:BoundaryStrategy}

    type IntervalSequence<'p, 'v> = { id:string; intvalues:IntervalValue<'p,'v> list }

    let instantaneousInterval pos v =
        InstantaneousIntervalValue {instant=pos;value=fun _->v} //TODO clamp value to normalized
    
    let pointPairToInterval<'p,'v> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->'p->'v) (bound:BoundaryStrategy) ptpair : IntervalValue<'p,'v> =
            let (sp:PointValue<'p, 'v>, ep:PointValue<'p,'v>) = ptpair
            let iv = interp ptpair
            match bound with
            | InclusiveLow -> FiniteIntervalValue {start=Inclusive sp.position;``end``=Exclusive ep.position; value=iv}
            | InclusiveHigh -> FiniteIntervalValue {start=Exclusive sp.position;``end``=Inclusive ep.position; value=iv}

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

    let forwardRayConstantValueFunc<'p,'v when 'p : comparison> (pstart:'p) (v:'v) (p:'p) : 'v =
        if pstart > p then v else failwith "Undefined"

    let backwardRayConstantValueFunc<'p,'v when 'p : comparison> (pend:'p when 'p : comparison) (v:'v) (p:'p) : 'v =
        if pend > p then v else failwith "Undefined"

    let extrapolateTo (extrap:ExtrapolationStrategy) (int:IntervalValue<'p,'v>) =
        match extrap with
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | FiniteIntervalValue fiv -> 
                let v = fiv.value fiv.start.position
                let rayend = invertBound fiv.start            
                let rayval = backwardRayConstantValueFunc fiv.start.position v
                Some (BackwardRayIntervalValue {``end``=rayend;value=rayval})
            | InstantaneousIntervalValue iiv -> 
                let rayend = Exclusive iiv.instant
                let rayval = backwardRayConstantValueFunc iiv.instant <| iiv.value iiv.instant
                Some (BackwardRayIntervalValue {``end``=rayend;value=rayval})
            | _ -> None //TODO: handling/sanitizing a sequence with no nonzero-length intervals

        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let extrapolateFrom (extrap:ExtrapolationStrategy) (int:IntervalValue<'p,'v>) =
        match extrap with
        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | FiniteIntervalValue fiv -> 
                let v = fiv.value fiv.``end``.position
                let raystart = invertBound fiv.``end``
                let rayval = forwardRayConstantValueFunc fiv.``end``.position v
                Some (ForwardRayIntervalValue {start=raystart;value=rayval})
            | InstantaneousIntervalValue iiv -> 
                let raystart = Exclusive iiv.instant
                let rayval = forwardRayConstantValueFunc iiv.instant <| iiv.value iiv.instant
                Some (ForwardRayIntervalValue {start=raystart;value=rayval})
            | _ -> None
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let applyExtrapolation (extrap:ExtrapolationStrategy) (intseq:IntervalValue<'p,'v> list) : IntervalValue<'p,'v> list  =
        let head = List.head intseq
        let bef = extrapolateTo extrap head
        let beffolder s ray = 
            match s with
            | InstantaneousIntervalValue _ :: t -> ray :: t
            | _ -> ray :: s
        let withbef = bef |> Option.fold beffolder intseq
        let aftfolder s ray =
            let last = Seq.tryLast withbef
            match last with
            | Some (InstantaneousIntervalValue _) -> 
                let trunc = List.truncate (intseq.Length - 1) intseq //remove last. cringe.
                List.append trunc [ray]
            | _ -> List.append s [ray]
        let aft = extrapolateFrom extrap <| List.last intseq //ouch
        aft |> Option.fold aftfolder withbef |> List.ofSeq

    let remodelPairwiseToIntervals<'p,'v> (interp:(PointValue<'p,'v>*PointValue<'p,'v>)->'p->'v) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v> =
        let ptPairs = Seq.pairwise ptseq.ptvalues 
        let ptsToInterval ptpair = pointPairToInterval interp ptseq.bound ptpair
        let lastPt = List.last ptseq.ptvalues
        let lastInt = instantaneousInterval lastPt.position <| lastPt.value
        let intervals = Seq.map ptsToInterval ptPairs |> Seq.append <| [lastInt] |> List.ofSeq
        {id=ptseq.id; intvalues=intervals}

    let remodelToLinearIntervals (interp:PointValue<'p,'v>*PointValue<'p,'v>->'p->'v) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v>  =
        let ints = remodelPairwiseToIntervals interp ptseq
        let intsextrap = applyExtrapolation ptseq.extrap ints.intvalues 
        { ints with intvalues = intsextrap }

    let remodelToConstantIntervals (interp:PointValue<'p,'v>->'p->'v) (ptseq:PointSequence<'p,'v>) : IntervalSequence<'p,'v> = //TODO exorcise the 'v as obj mess
        let interpwrap (boundvals:PointValue<'p,'v> * PointValue<'p,'v>) =
            let (s, _) = boundvals
            interp s
        let ints = remodelPairwiseToIntervals interpwrap ptseq
        let intsextrap = applyExtrapolation ptseq.extrap ints.intvalues 
        { ints with intvalues = intsextrap }

    let remodelToPoints (ancType:RemodelAnchor) (bound:BoundaryStrategy) (inseq:IntervalSequence<'a,'b>) : PointSequence<'a,'b> =

        let pointMapper (anct:RemodelAnchor) (intv:IntervalValue<'a,'b>) : PointValue<'a,'b> option =
            match intv with
            | FiniteIntervalValue fiv ->
                match anct with
                | IntervalStart -> Some {position=fiv.start.position;value=(fiv.value fiv.start.position)}
                | IntervalEnd -> Some {position=fiv.``end``.position;value=fiv.value fiv.``end``.position}
            | ForwardRayIntervalValue friv ->
                match anct with
                | IntervalStart -> Some {position=friv.start.position;value=friv.value friv.start.position}
                | _ -> None
            | BackwardRayIntervalValue briv ->
                match anct with
                | IntervalEnd -> Some {position=briv.``end``.position;value=briv.value briv.``end``.position}
                | _ -> None
            | InstantaneousIntervalValue iiv -> Some {position=iiv.instant;value=iiv.value iiv.instant}                     

        let hasfray =  Seq.exists (fun i -> match i with | ForwardRayIntervalValue _ -> true; | _ -> false) inseq.intvalues
        let hasbray = Seq.exists (fun i -> match i with | BackwardRayIntervalValue _ -> true; | _ -> false) inseq.intvalues
        let extrap = 
            match (hasfray, hasbray) with
            | (true, true) -> ExtrapolationStrategy.BeforeAndAfter
            | (true, false) -> ExtrapolationStrategy.BeforeFirst
            | (false, true) -> ExtrapolationStrategy.AfterLast
            | _ -> ExtrapolationStrategy.NoExtrapolation

        let ptvals = List.choose id <| List.map (pointMapper ancType) inseq.intvalues
        {id=inseq.id;extrap=extrap;ptvalues=ptvals;bound=bound}    

    let intervalEndsBefore (pos:'a) (intv:IntervalValue<'a,'b>) =
        match intv with
            | FiniteIntervalValue fiv -> pos |> isAfterBoundary fiv.``end``
            | BackwardRayIntervalValue briv -> pos  |> isAfterBoundary briv.``end``
            | InstantaneousIntervalValue iiv -> pos |> isAfterBoundary (Inclusive iiv.instant)
            | ForwardRayIntervalValue _ -> false

    let intervalStartsAfter (pos:'a) (intv:IntervalValue<'a,'b>) =
        match intv with
            | FiniteIntervalValue fiv -> pos |> isBeforeBoundary fiv.start
            | BackwardRayIntervalValue _ -> false
            | ForwardRayIntervalValue friv -> pos |> isBeforeBoundary friv.start
            | InstantaneousIntervalValue iiv -> pos |> isBeforeBoundary (Inclusive iiv.instant)

    let intervalContains (pos:'a) (intv:IntervalValue<'a,'b>)  =
        let notcontains = (intv |> intervalEndsBefore pos) && (intv |> intervalStartsAfter pos)
        not notcontains //I'm not not licking toads

    let intervalOptContains (pos:'a) (intop:IntervalValue<'a,'b> option) =
            match intop with
            | Some int -> intervalContains pos int
            | None -> false

    let getIntervalAtOrBefore (vals:IntervalValue<'a,'b> list) (pos:'a) =
        let rec atOrBefore pos rem (prev:IntervalValue<'a,'b> option) = 
            match rem with
            | [] -> None
            | (FiniteIntervalValue h) :: t -> if intervalStartsAfter pos (FiniteIntervalValue h) then prev else atOrBefore pos t (Some (FiniteIntervalValue h))
            | (BackwardRayIntervalValue h) :: t -> atOrBefore pos t (Some (BackwardRayIntervalValue h))
            | [(ForwardRayIntervalValue h)] -> if intervalStartsAfter pos (ForwardRayIntervalValue h)  then None else (Some (ForwardRayIntervalValue h))
            | (InstantaneousIntervalValue iiv) :: t -> if intervalStartsAfter pos (InstantaneousIntervalValue iiv) then prev else atOrBefore pos t (Some (InstantaneousIntervalValue iiv))
            | _ :: t -> atOrBefore pos t prev
        atOrBefore pos vals None        

    let getNearestPointInInterval (intv:IntervalValue<'a,'b> option) (pos:'a) : PointValue<'a,'b> option = //TODO this should clamp (?). Also, remove redundacy.
        match intv with //Hmmm... this implies a defined behavior for values interpolated outside bounds. No bueno.
        | Some (FiniteIntervalValue fiv) -> Some {position=pos;value=fiv.value pos}
        | Some (InstantaneousIntervalValue iiv) -> Some {position=pos;value=iiv.value pos}
        | Some (ForwardRayIntervalValue friv) -> Some {position=pos;value=friv.value pos}
        | Some (BackwardRayIntervalValue briv) -> Some {position=pos;value=briv.value pos}
        | None -> None
    
    let getExactPointInInterval (intop:IntervalValue<'a,'b> option) (pos:'a) : PointValue<'a,'b> option =
        let cont = intervalOptContains pos intop
        match cont with
        | true -> getNearestPointInInterval intop pos
        | false -> None

    let getPointInSequence (inseq:IntervalSequence<'a,'b>) (pos:'a) =
        let prev = getIntervalAtOrBefore inseq.intvalues pos
        getExactPointInInterval prev pos

    let SomeNonzeroLength (int:IntervalValue<'a,'b>) =
        let isNonzeroLength (fiv:FiniteIntervalValue<'a,'b>) = fiv.start <> fiv.``end``
        match int with
        | FiniteIntervalValue fiv -> if (isNonzeroLength fiv) then Some int else None
        | _ -> Some int //not finite so cannot be zero length

    let splitInterval (intop:IntervalValue<'a,'b> option) (bound:BoundaryStrategy) (pos:'a) =
        let endpto = getNearestPointInInterval intop pos
        let trim int bnd (mid:PointValue<'a,'b>) =  
            let (befmid, aftmid) =
                match bnd with
                | InclusiveLow -> (Exclusive mid.position, Inclusive mid.position)
                | InclusiveHigh -> (Inclusive mid.position, Exclusive mid.position)
            match int with
            | BackwardRayIntervalValue briv -> 
                {before=Some <| BackwardRayIntervalValue {``end``=befmid;value=briv.value};after=SomeNonzeroLength <| FiniteIntervalValue {start=aftmid;``end``=briv.``end``;value=briv.value}}
            | FiniteIntervalValue fiv -> 
                {before=SomeNonzeroLength <| FiniteIntervalValue {start=fiv.start;``end``=befmid;value=fiv.value};after=SomeNonzeroLength <| FiniteIntervalValue {start=aftmid;``end``=fiv.``end``;value=fiv.value}}
            | ForwardRayIntervalValue friv -> 
                {before=SomeNonzeroLength <| FiniteIntervalValue {start=friv.start;``end``=befmid;value=friv.value};after=Some <| ForwardRayIntervalValue {start=aftmid;value=friv.value}}
            | InstantaneousIntervalValue _ -> {before=Some int;after=Some int}

        let trimopt = Option.map3 trim
        Option.defaultValue {before=None;after=None} <| trimopt intop (Some bound) endpto     

    let trimIntervalTo (intop:IntervalValue<'a,'b> option) (pos:IntervalBoundary<'a>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveHigh
            | Exclusive _ -> BoundaryStrategy.InclusiveLow
        let split = splitInterval intop strat pos.position
        split.before

    let trimIntervalFrom (intop:IntervalValue<'a,'b> option) (pos:IntervalBoundary<'a>) =
        let strat = 
            match pos with
            | Inclusive _ -> BoundaryStrategy.InclusiveLow
            | Exclusive _ -> BoundaryStrategy.InclusiveHigh
        let split = splitInterval intop strat pos.position
        split.after

    let getIntervalsAtAndAfter  (inseq:IntervalSequence<'a,'b>) (strat:SliceStrategy) (pos:'a) =
        let skipper (intval:IntervalValue<'a,'b>) = intervalEndsBefore pos intval
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
                        let split = splitInterval shead InclusiveLow pos
                        match (split.after) with
                        | Some sint -> Seq.append [sint] skipped
                        | None -> skipped
                    | false -> skipped                
        List.ofSeq bounded

    let getIntervalsAtAndBefore (inseq:IntervalSequence<'a,'b>) (strat:SliceStrategy) (pos:'a) = 
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
                        let split = splitInterval (Some h) InclusiveHigh pos
                        match (split.after) with
                        | Some sint -> sint :: t
                        | None -> t
                    | false -> t     
                | [] -> []                           


    let sliceByInterval (s:IntervalSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = 
            match s.start with
            | Some start -> getIntervalsAtAndAfter inseq start.strategy start.position
            | None -> inseq.intvalues  
        let etrim =
            match s.``end`` with
            | Some e -> getIntervalsAtAndBefore {inseq with intvalues=strim} e.strategy e.position
            | None -> strim
        {inseq with intvalues=etrim}

    let sliceForwardByCount (s:ForwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = getIntervalsAtAndAfter inseq s.start.strategy s.start.position
        let ctrim = List.truncate s.count strim
        {inseq with intvalues=ctrim}

    let takeLast n seq =  //cringe
        let rev = Seq.rev seq
        let trim = Seq.take n rev
        Seq.rev trim |> List.ofSeq

    let sliceBackwardByCount (s:BackwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = getIntervalsAtAndBefore inseq s.``end``.strategy s.``end``.position
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

