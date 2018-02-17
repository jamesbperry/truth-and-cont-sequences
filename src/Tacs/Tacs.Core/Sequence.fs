namespace Tacs.Core

module Sequence =

    open Types

    type PointSequence<'p, 'v> = { id:string; interp:(FiniteIntervalValue<'p,'v> -> 'p -> PointValue<'p,'v>); extrap:ExtrapolationStrategy; ptvalues:PointValue<'p,'v> list }

    type IntervalSequence<'p, 'v> = { id:string; interp:(FiniteIntervalValue<'p,'v> -> 'p -> PointValue<'p,'v>); extrap:ExtrapolationStrategy; intvalues:IntervalValue<'p,'v> list } //TODO remove extrap val as redundant(?)

    let remodelToIntervals (ptseq:PointSequence<'a,'b>) : IntervalSequence<'a,'b> =

        let pairFolder v state =
            match state with
                | [] -> (v, Option<PointValue<'a,'b>>.None) :: state //will be discarded
                | (h ,None) :: tail -> (v,h ) :: tail //discards the above
                | (h,_) :: _ -> (v, h) :: state

        let ptPairs = Seq.foldBack pairFolder (ptseq.ptvalues |> Seq.map Some) []

        let toIntervals (ps,pe) =
            match (ps, pe) with
            | (Some is, Some pev) -> 
                let ie = ptseq.interp {start=is; ``end``=pev} pev.position
                FiniteIntervalValue {start=is; ``end``=ie}
            | (Some is, None) -> ForwardRayIntervalValue {start=is}
            | (None, Some ie) -> BackwardRayIntervalValue {``end``=ie}
            | _ -> failwithf "Invalid interval with no start or end"

        let intervals = List.map toIntervals ptPairs

        let withPreExtrap (ints:IntervalValue<'a,'b> list) =
            match ints with
            | (FiniteIntervalValue head) :: _ -> BackwardRayIntervalValue { ``end``=head.start } :: ints
            | _ -> ints

        //if "after" extrap, append open event. This whole method reeks... rewrite!
        let withPostExtrap (ints:IntervalValue<'a,'b> list) =
            let last = ints |> List.last //eww
            match last with
            | FiniteIntervalValue lastf -> [ForwardRayIntervalValue { start = lastf.``end``}] |> List.append ints //eww
            | _ -> ints

        let intervalsExtrap =
            match ptseq.extrap with
            | ExtrapolationStrategy.BeforeFirst -> intervals |> withPreExtrap
            | ExtrapolationStrategy.AfterLast -> intervals |> withPostExtrap
            | ExtrapolationStrategy.BeforeAndAfter -> intervals |> withPreExtrap |> withPostExtrap
            | _ -> intervals

        {id=ptseq.id; interp=ptseq.interp; extrap=ptseq.extrap; intvalues=intervalsExtrap}

    let remodelToPoints (ancType:RemodelAnchor) (inseq:IntervalSequence<'a,'b>) : PointSequence<'a,'b> =

        let pointMapper (anct:RemodelAnchor) (extrap:ExtrapolationStrategy) (intv:IntervalValue<'a,'b>) =
            match intv with
            | FiniteIntervalValue fiv ->
                match anct with
                | IntervalStart -> Some {position=fiv.start.position;value=fiv.start.value}
                | IntervalEnd -> Some {position=fiv.``end``.position;value=fiv.start.value}
            | ForwardRayIntervalValue friv ->
                match (anct, extrap) with
                | (IntervalStart, (ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter)) -> 
                    Some {position=friv.start.position;value=friv.start.value}
                | _ -> None
            | BackwardRayIntervalValue briv ->
                match (anct, extrap) with
                | (IntervalEnd, (ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.BeforeAndAfter)) -> 
                    Some {position=briv.``end``.position;value=briv.``end``.value}
                | _ -> None                

        //TODO optionally set ptseq extrapolation based on unbounded start/end intervals ?

        let ptvals = List.choose id <| List.map (pointMapper ancType inseq.extrap) inseq.intvalues

        {id=inseq.id;extrap=inseq.extrap;interp=inseq.interp;ptvalues=ptvals}    
    let intervalEndsBefore (pos:'a) (int:IntervalValue<'a,'b>) =
        match int with
            | FiniteIntervalValue fiv -> fiv.``end``.position <= pos
            | BackwardRayIntervalValue briv -> briv.``end``.position <= pos
            | ForwardRayIntervalValue _ -> false
    let intervalStartsAfter (pos:'a) (int:IntervalValue<'a,'b>) =
        match int with
            | FiniteIntervalValue fiv -> fiv.start.position > pos
            | BackwardRayIntervalValue _ -> false
            | ForwardRayIntervalValue friv -> friv.start.position > pos

    let intervalContains (pos:'a) (int:IntervalValue<'a,'b>)  =
        match int with
            | FiniteIntervalValue fiv -> (fiv.start.position <= pos) && (fiv.``end``.position > pos)
            | BackwardRayIntervalValue briv -> briv.``end``.position > pos
            | ForwardRayIntervalValue friv -> friv.start.position <= pos

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
            | _ :: t -> atOrBefore pos t prev
        atOrBefore pos vals None        

    let getNearestPointInInterval (interp) (int:IntervalValue<'a,'b> option) (pos:'a) : PointValue<'a,'b> option =
        match int with
        | Some (FiniteIntervalValue fiv) -> Some (interp fiv pos)
        | Some (ForwardRayIntervalValue friv) -> Some {position=pos;value=friv.start.value}
        | Some (BackwardRayIntervalValue briv) -> Some {position=pos;value=briv.``end``.value}
        | None -> None
    
    let getExactPointInInterval (interp) (intop:IntervalValue<'a,'b> option) (pos:'a) : PointValue<'a,'b> option =
        let cont = intervalOptContains pos intop
        match cont with
        | true -> getNearestPointInInterval interp intop pos
        | false -> None

    let getPointInSequence (inseq:IntervalSequence<'a,'b>) (pos:'a) =
        let prev = getIntervalAtOrBefore inseq.intvalues pos
        getExactPointInInterval inseq.interp prev pos

    let SomeNonzeroLength (int:IntervalValue<'a,'b>) =
        let isNonzeroLength (fiv:FiniteIntervalValue<'a,'b>) = fiv.start.position <> fiv.``end``.position
        match int with
        | FiniteIntervalValue fiv -> if (isNonzeroLength fiv) then Some int else None
        | _ -> Some int //not finite so cannot be zero length

    let splitInterval (interp) (intop:IntervalValue<'a,'b> option) (pos:'a) =
        let endpto = getNearestPointInInterval interp intop pos
        let trim int endpt =  
            match int with
            | BackwardRayIntervalValue briv -> 
                {before=Some <| BackwardRayIntervalValue {``end``=endpt};after=SomeNonzeroLength <| FiniteIntervalValue {start=endpt;``end``=briv.``end``}}
            | FiniteIntervalValue fiv -> 
                {before=SomeNonzeroLength <| FiniteIntervalValue {start=fiv.start;``end``=endpt};after=SomeNonzeroLength <| FiniteIntervalValue {start=endpt;``end``=fiv.``end``}}
            | ForwardRayIntervalValue friv -> 
                {before=SomeNonzeroLength <| FiniteIntervalValue {start=friv.start;``end``=endpt};after=Some <| ForwardRayIntervalValue {start=endpt}}           
        let trimopt = Option.map2 trim
        Option.defaultValue {before=None;after=None} <| trimopt intop endpto       

    let trimIntervalTo (interp) (intop:IntervalValue<'a,'b> option) (pos:'a) =
        let split = splitInterval interp intop pos
        split.before

    let trimIntervalFrom (interp) (intop:IntervalValue<'a,'b> option) (pos:'a) =
        let split = splitInterval interp intop pos
        split.after

    let extrapolateTo (extrap:ExtrapolationStrategy) (int:IntervalValue<'a,'b>) =
        match extrap with
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | BackwardRayIntervalValue _ -> None
            | FiniteIntervalValue fiv -> Some (BackwardRayIntervalValue {``end``=fiv.start})
            | ForwardRayIntervalValue _ -> None //TODO: handling/sanitizing a sequence with no nonzero-length intervals
        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let extrapolateFrom (extrap:ExtrapolationStrategy) (int:IntervalValue<'a,'b>) =
        match extrap with
        | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter -> 
            match int with
            | BackwardRayIntervalValue _ -> None
            | FiniteIntervalValue fiv -> Some (ForwardRayIntervalValue {start=fiv.``end``})
            | ForwardRayIntervalValue _ -> None //TODO: handling/sanitizing a sequence with no nonzero-length intervals
        | ExtrapolationStrategy.BeforeFirst | ExtrapolationStrategy.NoExtrapolation -> None
        | _ -> failwith "Behavior undefined for extrapolation strategy"

    let extrapolateListFrom extrap int = 
        let efn = Option.bind <| extrapolateFrom extrap
        let ei = efn int
        match ei with
        | Some e -> [e]
        | None -> []

    let removeExtrapolation (invals:IntervalValue<'a,'b> list) : (FiniteIntervalValue<'a,'b> list) =
        let tryFinite iv =
            match iv with
            | FiniteIntervalValue fiv -> Some fiv
            | _ -> None
        List.choose tryFinite invals

    let getIntervalsAtAndAfter  (inseq:IntervalSequence<'a,'b>) (strat:BoundaryStrategy) (pos:'a) =
        let rec trimmer (p:'a) (rem:IntervalValue<'a,'b> list) =
            match rem with
            | [] -> []
            | ForwardRayIntervalValue _ :: _ -> rem
            | iv :: t -> if (intervalContains p iv) then rem else (trimmer p t)
        let trimmed = trimmer pos inseq.intvalues
        let withfirst = 
            match (trimmed, strat) with                     
                | ([],_) -> []
                | (h::t,Interpolated) -> 
                    let trimop = trimIntervalFrom inseq.interp (Some h) pos
                    match trimop with
                    | Some trim -> trim :: t
                    | None -> t
                | (_::t,Inside) -> t    
                | (h::t,Intersected) -> h::t
        match withfirst with
            | [] -> withfirst
            | (h::t) -> 
                let extrap = extrapolateTo inseq.extrap h
                match extrap with
                | Some ei -> ei::withfirst
                | None -> withfirst              

    let getIntervalsAtAndBefore (inseq:IntervalSequence<'a,'b>) (strat:BoundaryStrategy) (pos:'a) = 
        let extrap = extrapolateListFrom inseq.extrap
        let rec fslicer p (rem:IntervalValue<'a,'b> list) (prev:IntervalValue<'a,'b> option) =
            seq {
                match rem with
                | [] -> 
                    yield! extrap prev
                | (h::t) -> match (intervalContains p h) with 
                                | false ->
                                    yield h 
                                    yield! fslicer p t (Some h) 
                                | true ->
                                    match strat with
                                    | Interpolated -> 
                                        let trimop = trimIntervalTo inseq.interp (Some h) pos
                                        match trimop with
                                        | Some trim -> 
                                            yield trim
                                            yield! extrap trimop
                                        | None ->
                                            yield! extrap prev
                                    | Inside ->
                                        yield! extrap prev
                                    | Intersected -> 
                                        yield h
                                        yield! extrap (Some h)
            }        
        List.ofSeq <| fslicer pos inseq.intvalues None

    let sliceByInterval (s:IntervalSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = 
            match s.start with
            | Some start -> getIntervalsAtAndAfter inseq start.strategy start.location
            | None -> inseq.intvalues  
        let etrim =
            match s.``end`` with
            | Some e -> getIntervalsAtAndBefore {inseq with intvalues=strim} e.strategy e.location
            | None -> strim
        {inseq with intvalues=etrim}

    let sliceForwardByCount (s:ForwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let strim = getIntervalsAtAndAfter inseq s.start.strategy s.start.location
        let extrap = extrapolateListFrom inseq.extrap
        let rec getcount ints prev n j =
            seq {
                match ints with
                | [] -> yield! extrap prev
                | (h::t) ->
                    let jprime =
                        match h with
                        | FiniteIntervalValue _ -> j + 1
                        | _ -> j
                    if (jprime <= n) then
                        yield h
                        yield! getcount t (Some h) n jprime
                    else
                        yield! extrap prev                     
            }        
        let ctrim = List.ofSeq <| getcount strim None s.count 0

        {inseq with intvalues=ctrim}

    let sliceBackwardByCount (s:BackwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        let fiSlice = {IntervalSlice.start=None;IntervalSlice.``end``=Some s.``end``}
        let trimseq = sliceByInterval fiSlice inseq
        let tvr = List.rev trimseq.intvalues
        let count =
            let rc = 
                match inseq.extrap with
                | ExtrapolationStrategy.AfterLast | ExtrapolationStrategy.BeforeAndAfter -> s.count + 1
                | _ -> s.count
            System.Math.Min (tvr.Length, rc)
        let svr = List.take count tvr
        let sv = List.rev svr
        let sve =
            match sv with
            | [] -> []
            | (h::t) -> 
                let beo = extrapolateTo inseq.extrap h
                match beo with
                | Some be -> be :: sv
                | _ -> sv
        {inseq with intvalues=sve}

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
    //         let ebound = last.``end``.position
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

