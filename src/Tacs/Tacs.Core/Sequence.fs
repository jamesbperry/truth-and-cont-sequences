namespace Tacs.Core

module Sequence =

    open Types

    type PointSequence<'p, 'v> = { id:string; interp:(FiniteIntervalValue<'p,'v> -> 'p -> PointValue<'p,'v>); extrap:ExtrapolationStrategy; ptvalues:PointValue<'p,'v> list }

    type IntervalSequence<'p, 'v> = { id:string; interp:(FiniteIntervalValue<'p,'v> -> 'p -> PointValue<'p,'v>); extrap:ExtrapolationStrategy; intvalues:IntervalValue<'p,'v> list }

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

    let intervalContains (int:IntervalValue<'a,'b> option) (pos:'a) =
            match int with
            | Some (FiniteIntervalValue fiv) -> (fiv.start.position <= pos) && (fiv.``end``.position >= pos)
            | Some (BackwardRayIntervalValue briv) -> briv.``end``.position >= pos
            | Some (ForwardRayIntervalValue friv) -> friv.start.position <= pos
            | _ -> false

    let getIntervalAtOrBefore (vals:IntervalValue<'a,'b> list) (pos:'a) =
        let rec atOrBefore pos rem (prev:IntervalValue<'a,'b> option) = 
            match rem with
            | [] -> None
            | (FiniteIntervalValue h) :: t -> if h.start.position > pos then prev else atOrBefore pos t (Some (FiniteIntervalValue h))
            | (BackwardRayIntervalValue h) :: t -> atOrBefore pos t (Some (BackwardRayIntervalValue h))
            | [(ForwardRayIntervalValue h)] -> if h.start.position > pos then (Some (ForwardRayIntervalValue h)) else None
            | _ :: t -> atOrBefore pos t prev
        atOrBefore pos vals None        

    let getPointInInterval (interp) (int:IntervalValue<'a,'b> option) (pos:'a) : PointValue<'a,'b> option =

        let contained = intervalContains int pos
        let intopt = if contained then int else None
        let v =
            match intopt with
            | Some (FiniteIntervalValue fiv) -> Some (interp fiv pos)
            | Some (ForwardRayIntervalValue friv) -> Some {position=pos;value=friv.start.value}
            | Some (BackwardRayIntervalValue briv) -> Some {position=pos;value=briv.``end``.value}
            | _ -> None
        v
    let getPointInSequence (inseq:IntervalSequence<'a,'b>) (pos:'a) =
        let prev = getIntervalAtOrBefore inseq.intvalues pos
        getPointInInterval inseq.interp prev pos

    // let sliceForwardByCount (b:ForwardSlice<'a>) (inseq:IntervalSequence<'a,'b>) : (IntervalSequence<'a,'b>) =
        
    //     getValue inseq.interp 

    //     let rec filter bound vals =
    //         match vals with
    //         | [] ->
    //         | h::(n::t) ->
    //             match h with
    //             | {Some start, Some ``end``} ->
    //             | 
    //         | [h,t] ->

        
