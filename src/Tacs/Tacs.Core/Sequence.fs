namespace Tacs.Core

module Sequence =

    open Types

    let remodel (inseq:PointSequence<'a,'b>) : IntervalSequence<'a,'b> =

        let pairFolder v state =
            match state with
                | [] -> (v, Option<PointValue<'a,'b>>.None) :: state //will be discarded
                | (h ,None) :: tail -> (v,h ) :: tail //discards the above
                | (h,_) :: _ -> (v, h) :: state

        let ptPairs = Seq.foldBack pairFolder (inseq.ptvalues |> Seq.map Some) []

        let toIntervals (ps,pe) =
            let ie = match inseq.interp with
                | Step ->
                    match (ps,pe) with
                        | (Some s, Some e) -> Some {position=e.position; value=s.value}
                        | (_,_) -> None
                | Linear -> pe
            {start=ps; ``end``=ie} 

        let intervals = List.map toIntervals ptPairs

        let withPreExtrap ints =
            match ints with
            | head :: _ -> { start=None; ``end``=head.start } :: ints
            | _ -> ints

        //if "after" extrap, append open event. This whole method reeks... rewrite!
        let withPostExtrap ints =
            let last = ints |> List.last //eww
            [{ start = last.``end``; ``end``=None }] |> List.append ints //eww

        let intervalsExtrap =
            match inseq.extrap with
            | ExtrapolationStrategy.BeforeFirst -> intervals |> withPreExtrap
            | ExtrapolationStrategy.AfterLast -> intervals |> withPostExtrap
            | ExtrapolationStrategy.BeforeAndAfter -> intervals |> withPreExtrap |> withPostExtrap
            | _ -> intervals

        {id=inseq.id; interp=inseq.interp; extrap=inseq.extrap; intvalues=intervalsExtrap}
