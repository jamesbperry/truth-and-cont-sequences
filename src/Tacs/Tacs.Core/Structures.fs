// namespace Tacs.Core

// module Structures =

//     open Tacs.Core.Types
//     open System.Collections
//     open System.Collections.Generic

//     type ISequenceValues<'a,'b> =
//         inherit IEnumerable
//         inherit IEnumerable<Interval<'a,'b>>
//         abstract member skipWhile : (Interval<'a,'b> -> bool) -> ISequenceValues<'a,'b>
//         abstract member takeWhile : (Interval<'a,'b> -> bool) -> ISequenceValues<'a,'b>
//         abstract member cons : Interval<'a,'b> -> ISequenceValues<'a,'b>
//         abstract member snoc : Interval<'a,'b> -> ISequenceValues<'a,'b>
//         abstract member unCons : Interval<'a,'b> * ISequenceValues<'a,'b>
//         abstract member unSnoc : ISequenceValues<'a,'b> * Interval<'a,'b>
//         abstract member ofSeq : Interval<'a,'b> seq -> ISequenceValues<'a,'b>
//         abstract member ofList : Interval<'a,'b> list -> ISequenceValues<'a,'b>
    
    // type ListSequenceValues<'a,'b>(listvals:IntervalValue<'a,'b> list) =
    //     new(seqvals:IntervalValue<'a,'b> seq) =
    //         let lv = List.ofSeq seqvals
    //         ListSequenceValues(lv)
    //     member private this.vals : IntervalValue<'a,'b> list = listvals
    //     interface ISequenceValues<'a,'b> with
    //         member this.ofList(ls) = new ListSequenceValues<'a,'b>(ls) :> ISequenceValues<'a,'b>
    //         member this.skipWhile(p) = this.ofList <| List.skipWhile (fun iv -> p (iv this.vals
    //     end