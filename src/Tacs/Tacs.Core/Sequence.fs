namespace Tacs.Core

module Sequence =

    open Types

    let remodelToIntervals (ptseq:PointSequence<'a,'b>) : IntervalSequence<'a,'b> =

        let pairFolder v state =
            match state with
                | [] -> (v, Option<PointValue<'a,'b>>.None) :: state //will be discarded
                | (h ,None) :: tail -> (v,h ) :: tail //discards the above
                | (h,_) :: _ -> (v, h) :: state

        let ptPairs = Seq.foldBack pairFolder (ptseq.ptvalues |> Seq.map Some) []

        let toIntervals (ps,pe) =
            let ie = 
                match ptseq.interp with
                | Step ->
                    match (ps,pe) with
                        | (Some s, Some e) -> Some {position=e.position; value=s.value}
                        | (_,_) -> None
                | Linear -> pe
            match (ps, ie) with
            | (Some is, Some ie) -> FiniteIntervalValue {start=is; ``end``=ie}
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