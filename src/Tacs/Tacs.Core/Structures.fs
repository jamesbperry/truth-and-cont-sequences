namespace Tacs.Core

module Structures =

    open Tacs.Core.Types
    open System.Collections
    open System.Collections.Generic

    type ISequenceValues<'a,'b> =
        inherit IEnumerable
        inherit IEnumerable<IntervalValue<'a,'b>>
        abstract member skipWhile : (IntervalValue<'a,'b> -> bool) -> ISequenceValues<'a,'b>
        abstract member takeWhile : (IntervalValue<'a,'b> -> bool) -> ISequenceValues<'a,'b>
        abstract member cons : IntervalValue<'a,'b> -> ISequenceValues<'a,'b>
        abstract member snoc : IntervalValue<'a,'b> -> ISequenceValues<'a,'b>
        abstract member unCons : IntervalValue<'a,'b> * ISequenceValues<'a,'b>
        abstract member unSnoc : ISequenceValues<'a,'b> * IntervalValue<'a,'b>
        abstract member ofSeq : IntervalValue<'a,'b> seq -> ISequenceValues<'a,'b>
        abstract member ofList : IntervalValue<'a,'b> list -> ISequenceValues<'a,'b>
    
    // type ListSequenceValues<'a,'b>(listvals:IntervalValue<'a,'b> list) =
    //     new(seqvals:IntervalValue<'a,'b> seq) =
    //         let lv = List.ofSeq seqvals
    //         ListSequenceValues(lv)
    //     member private this.vals : IntervalValue<'a,'b> list = listvals
    //     interface ISequenceValues<'a,'b> with
    //         member this.ofList(ls) = new ListSequenceValues<'a,'b>(ls) :> ISequenceValues<'a,'b>
    //         member this.skipWhile(p) = this.ofList <| List.skipWhile (fun iv -> p (iv this.vals
    //     end      



