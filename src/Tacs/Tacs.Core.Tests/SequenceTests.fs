module SequenceTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types
open Tacs.Core.Sequence
open Tacs.Core
open Tacs.Core.IntegerOps
open Tacs.Core.FloatOps
open Tacs.Core.TimeOps
open System

[<Test>]
let ``points to intervals should be reversible`` () =
    let ptvals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let ptseq = {id="test";extrap=BeforeAndAfter;bound=InclusiveLow;ptvalues=ptvals}
    let intseq = remodelPairwiseToIntervals IntegerPosition LinearNearestIntValue ptseq
    let ptseq2 = remodelToPoints IntegerPosition InclusiveLow intseq
    ptseq2.ptvalues |> should equal ptseq.ptvalues

[<Test>]
let ``intervals to points should be reversible`` () =
    let intvals = [
        LinearNearestIntInterval ({position=Inclusive 1;value=1},{position=Exclusive 2;value=2})
        LinearNearestIntInterval ({position=Inclusive 2;value=2},{position=Exclusive 3;value=3})
        LinearNearestIntInterval ({position=Inclusive 3;value=3},{position=Exclusive 4;value=4})
        LinearNearestIntInterval ({position=Inclusive 4;value=4},{position=Inclusive 5;value=5})];
    let intseq = {id="test"; intvalues=intvals;preextrap=ConstantBefore (Exclusive 1) 1;postextrap=ConstantAfter (Exclusive 5) 5}
    let ptseq = remodelToPoints IntegerPosition InclusiveLow intseq
    let intseq2 = remodelPairwiseToIntervals IntegerPosition LinearNearestIntValue ptseq
    (Array.ofList intseq2.intvalues) |> should equal (Array.ofList intseq.intvalues)

[<Test>]
let ``linear interpolation should produce correct value for int positions and float values`` () =
    let inter = FloatOps.LinearFloatInterval ({position=Inclusive 1;value=1.0},{position=Exclusive 100;value=100.0})
    let pv = inter.ValueAt IntegerPosition 42
    pv |> should equal 42.0

[<Test>]
let ``linear interpolation should produce correct value for int positions and int values`` () =
    let inter = LinearNearestIntInterval ({position=Inclusive 1;value=1},{position=Exclusive 100;value=100})
    let pv = inter.ValueAt IntegerPosition 42
    pv |> should equal 42

[<Test>]
let ``linear interpolation should produce correct value for float positions and float values`` () =
    let inter = LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 100.0;value=100.0})
    let pv = inter.ValueAt FloatPosition 42.0
    pv |> should equal 42.0


[<Test>]
let ``interpolation should work in middle of sequences`` () =
    let intvals = [
        LinearFloatInterval ({position=Inclusive 1;value=1.0},{position=Exclusive 2;value=2.0});
        LinearFloatInterval ({position=Inclusive 2;value=2.0},{position=Exclusive 5;value=5.0});
        LinearFloatInterval ({position=Inclusive 5;value=5.0},{position=Exclusive 6;value=6.0});];
    let intseq = {id="test"; intvalues=intvals; preextrap=ConstantBeforeExclusive 1 1.0;postextrap=ConstantAfterInclusive 6 6.0 }
    let iv = getPointInSequence intseq IntegerPosition 4
    iv |> should equal (Some {position=4;value=4.0})

[<Test>]
let ``interpolation shouldn't find a value where there are none`` () =
    let intvals = [
        LinearFloatInterval ({position=Inclusive 1;value=1.0},{position=Exclusive 2;value=2.0});
        LinearFloatInterval ({position=Inclusive 2;value=2.0},{position=Exclusive 3;value=3.0});
        LinearFloatInterval ({position=Inclusive 5;value=5.0},{position=Exclusive 6;value=6.0});];
    let intseq = {id="test"; intvalues=intvals; preextrap=ConstantBeforeExclusive 1 1.0;postextrap=ConstantAfterInclusive 6 6.0 }
    let iv = getPointInSequence intseq IntegerPosition 4
    iv |> should equal None


[<Test>]
let ``interval slice on point positions should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = { IntervalSlice.start=Some {boundary=Inclusive 2.0;strategy=Interpolated};endbound= Some {boundary=Inclusive 4.0;strategy=Interpolated}}   
    let subseq = sliceByInterval FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``interval slice between point positions should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = { IntervalSlice.start=Some {boundary=Inclusive 1.5;strategy=Interpolated};endbound= Some {boundary=Inclusive 3.5;strategy=Interpolated}}   
    let subseq = sliceByInterval FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 1.5;value=1.5},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Inclusive 3.5;value=3.5});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``unbounded-end interval slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});]; 
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = { IntervalSlice.start=Some {boundary=Inclusive 3.5;strategy=Interpolated};endbound= None}   
    let subseq = sliceByInterval FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 3.5;value=3.5},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward interpolated slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {ForwardSlice.start={boundary=Inclusive 1.5;strategy=Interpolated};count=3}   
    let subseq = sliceForwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 1.5;value=1.5},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward intersected slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Exclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {ForwardSlice.start={boundary=Inclusive 1.5;strategy=SliceStrategy.Intersected};count=3}   
    let subseq = sliceForwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward inside slice should produce correct subsequence`` () = //TODO test boundary inclusivity conditions with more tests
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {ForwardSlice.start={boundary=Inclusive 1.5;strategy=SliceStrategy.Inside};count=2}   
    let subseq = sliceForwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``overflowing forward inside slice should produce correct subsequence`` () =

    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {ForwardSlice.start={boundary=Inclusive 0.5;strategy=SliceStrategy.Inside};count=200}   
    let subseq = sliceForwardByCount FloatPosition slstrat fullseq
    let expectedvals = allvals

    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)

[<Test>]
let ``backward interpolated slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0})];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {BackwardSlice.endbound={boundary=Inclusive 3.5;strategy=Interpolated};count=2}   
    let subseq = sliceBackwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Inclusive 3.5;value=3.5});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``backward intersected slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];  
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {BackwardSlice.endbound={boundary=Inclusive 3.5;strategy=SliceStrategy.Intersected};count=2}   
    let subseq = sliceBackwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``backward inside slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {BackwardSlice.endbound={boundary=Inclusive 4.5;strategy=SliceStrategy.Inside};count=2}   
    let subseq = sliceBackwardByCount FloatPosition slstrat fullseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``overflowing backward inside slice should produce correct subsequence`` () =
    let allvals = [
        LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=2.0},{position=Exclusive 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=3.0},{position=Exclusive 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive 4.0;value=4.0},{position=Inclusive 5.0;value=5.0});];
    let fullseq = {id="test"; intvalues=allvals; preextrap=ConstantBeforeExclusive 1.0 1.0;postextrap=ConstantAfterExclusive 5.0 5.0 }
    let slstrat = {BackwardSlice.endbound={boundary=Inclusive 5.5;strategy=SliceStrategy.Inside};count=200}   
    let subseq = sliceBackwardByCount FloatPosition slstrat fullseq
    let expectedvals = allvals
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)

[<Test>]
let ``unextrapolated evenly-spaced float integral should produce accurate results`` () =
    let inpvals = [
        LinearFloatInterval ({position=Inclusive 0.0;value=0.0},{position=Exclusive 1.0;value=2.0});   // linear 2*x^2     
        LinearFloatInterval ({position=Inclusive 1.0;value=2.0},{position=Exclusive 2.0;value=8.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=8.0},{position=Exclusive 3.0;value=18.0});
        LinearFloatInterval ({position=Inclusive 3.0;value=18.0},{position=Inclusive 4.0;value=32.0});]
    let inpseq:FloatValuedSequence<float> = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = FloatOps.Total OnFloatPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0.0, Exclusive 1.0) 1.0;    
        ConstantFloatInterval (Inclusive 1.0, Exclusive 2.0) 6.0;
        ConstantFloatInterval (Inclusive 2.0, Exclusive 3.0) 19.0;
        ConstantFloatInterval (Inclusive 3.0, Inclusive 4.0) 44.0;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced linear float integral should produce accurate results`` () =
    let inpvals = [
        LinearFloatInterval ({position=Inclusive 0.0;value=0.0},{position=Exclusive 1.0;value=2.0});   // linear 2*x^2     
        LinearFloatInterval ({position=Inclusive 1.0;value=2.0},{position=Exclusive 2.0;value=8.0});
        LinearFloatInterval ({position=Inclusive 2.0;value=8.0},{position=Inclusive 4.0;value=32.0});]
    let inpseq:FloatValuedSequence<float> = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = FloatOps.Total OnFloatPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0.0, Exclusive 1.0) 1.0;    
        ConstantFloatInterval (Inclusive 1.0, Exclusive 2.0) 6.0;
        ConstantFloatInterval (Inclusive 2.0, Inclusive 4.0) 46.0;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced mixed-behavior float integral should produce accurate results`` () =
    let inpvals = [
        LinearFloatInterval ({position=Inclusive 0.0;value=0.0},{position=Exclusive 1.0;value=2.0});   // linear 2*x^2     
        LinearFloatInterval ({position=Inclusive 1.0;value=2.0},{position=Exclusive 2.0;value=8.0});
        ConstantFloatInterval (Inclusive 2.0,Inclusive 4.0) 8.0;]
    let inpseq:FloatValuedSequence<float> = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = FloatOps.Total OnFloatPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0.0, Exclusive 1.0) 1.0;    
        ConstantFloatInterval (Inclusive 1.0, Exclusive 2.0) 6.0;
        ConstantFloatInterval (Inclusive 2.0, Inclusive 4.0) 22.0;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated evenly-spaced nearest-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearNearestIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});   // linear 2*x^2     
        LinearNearestIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=8});
        LinearNearestIntInterval ({position=Inclusive 2;value=8},{position=Exclusive 3;value=18});
        LinearNearestIntInterval ({position=Inclusive 3;value=18},{position=Inclusive 4;value=32});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0,Exclusive 1) 1.0;    
        ConstantFloatInterval (Inclusive 1,Exclusive 2) 6.0;
        ConstantFloatInterval (Inclusive 2,Exclusive 3) 19.0;
        ConstantFloatInterval (Inclusive 3,Inclusive 4) 44.0;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced nearest-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearNearestIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});   // linear 2*x^2     
        LinearNearestIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=8});
        LinearNearestIntInterval ({position=Inclusive 2;value=8},{position=Inclusive 4;value=32});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0, Exclusive 1) 1.0;    
        ConstantFloatInterval (Inclusive 1, Exclusive 2) 6.0;
        ConstantFloatInterval (Inclusive 2, Inclusive 4) 46.0;]
    Array.ofList integvals |> should equal expectedvals

[<Test>]
let ``unextrapolated evenly-spaced floor-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearFloorIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});       
        LinearFloorIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=2});
        LinearFloorIntInterval ({position=Inclusive 2;value=2},{position=Exclusive 3;value=4});
        LinearFloorIntInterval ({position=Inclusive 3;value=4},{position=Inclusive 4;value=0});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0, Exclusive 1) 0.5;    
        ConstantFloatInterval (Inclusive 1, Exclusive 2) 2.5;
        ConstantFloatInterval (Inclusive 2, Exclusive 3) 5.0;
        ConstantFloatInterval (Inclusive 3, Inclusive 4) 6.5;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced floor-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearFloorIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});       
        LinearFloorIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=2});
        LinearFloorIntInterval ({position=Inclusive 2;value=2},{position=Inclusive 4;value=0});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0, Exclusive 1) 0.5;    
        ConstantFloatInterval (Inclusive 1, Exclusive 2) 2.5;
        ConstantFloatInterval (Inclusive 2, Inclusive 4) 3.5;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated evenly-spaced ceiling-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearCeilingIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});       
        LinearCeilingIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=2});
        LinearCeilingIntInterval ({position=Inclusive 2;value=2},{position=Exclusive 3;value=4});
        LinearCeilingIntInterval ({position=Inclusive 3;value=4},{position=Inclusive 4;value=0});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0,Exclusive 1) 1.5;    
        ConstantFloatInterval (Inclusive 1,Exclusive 2) 3.5;
        ConstantFloatInterval (Inclusive 2,Exclusive 3) 7.0;
        ConstantFloatInterval (Inclusive 3,Inclusive 4) 9.5;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced ceiling-integer integral should produce accurate results`` () =
    let inpvals = [
        LinearCeilingIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 1;value=2});       
        LinearCeilingIntInterval ({position=Inclusive 1;value=2},{position=Exclusive 2;value=2});
        LinearCeilingIntInterval ({position=Inclusive 2;value=2},{position=Inclusive 4;value=0});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0,Exclusive 1) 1.5;    
        ConstantFloatInterval (Inclusive 1,Exclusive 2) 3.5;
        ConstantFloatInterval (Inclusive 2,Inclusive 4) 6.5;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``unextrapolated unevenly-spaced mixed-behavior integer total should produce accurate results`` () =
    let inpvals = [
        LinearCeilingIntInterval ({position=Inclusive 0;value=0},{position=Exclusive 2;value=2});       
        LinearFloorIntInterval ({position=Inclusive 2;value=2},{position=Exclusive 5;value=7});
        LinearNearestIntInterval ({position=Inclusive 5;value=7},{position=Exclusive 9;value=1});
        ConstantIntInterval (Inclusive 9,Inclusive 11) 2 ];
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = IntegerOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0,Exclusive 2) 3.0;    
        ConstantFloatInterval (Inclusive 2,Exclusive 5) 15.0;
        ConstantFloatInterval (Inclusive 5,Exclusive 9) 31.0;
        ConstantFloatInterval (Inclusive 9,Inclusive 11) 35.0;]
    Array.ofList integvals |> should equal expectedvals

[<Test>]
let ``aggregation with position of differing type should produce accurate results`` () =
    let inpvals = [
        LinearFloatInterval ({position=Inclusive 0;value=0.0},{position=Exclusive 1;value=2.0});   // linear 2*x^2     
        LinearFloatInterval ({position=Inclusive 1;value=2.0},{position=Exclusive 2;value=8.0});
        LinearFloatInterval ({position=Inclusive 2;value=8.0},{position=Inclusive 4;value=32.0});]
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let integvals  = FloatOps.Total OnIntPosition inpseq
    let expectedvals = [
        ConstantFloatInterval (Inclusive 0,Exclusive 1) 1.0;
        ConstantFloatInterval (Inclusive 1,Exclusive 2) 6.0;
        ConstantFloatInterval (Inclusive 2,Inclusive 4) 46.0;]
    Array.ofList integvals |> should equal expectedvals  

[<Test>]
let ``windowing by width from start looking forward should produce accurate results`` () =
    let rt = DateTimeOffset.FromUnixTimeSeconds <| int64 1531525039
    let rtp mins = rt + TimeSpan.FromMinutes mins
    let inpvals = [
        LinearFloatInterval ({position=Inclusive <| rtp 1.0;value=1.0},{position=Exclusive <| rtp 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive <| rtp 2.0;value=2.0},{position=Exclusive <| rtp 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive <| rtp 3.0;value=3.0},{position=Exclusive <| rtp 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive <| rtp 4.0;value=4.0},{position=Inclusive <| rtp 5.0;value=5.0});]; 
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let windowing = Tumbling (Start,LookingForward,TimeSpan.FromMinutes 1.0)
    let windowed = List.collect <| (fun s -> s.intvalues) <| Sequence.window TimePosition windowing inpseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive <| rtp 1.0;value=1.0},{position=Exclusive <| rtp 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive <| rtp 2.0;value=2.0},{position=Exclusive <| rtp 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive <| rtp 3.0;value=3.0},{position=Exclusive <| rtp 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive <| rtp 4.0;value=4.0},{position=Exclusive <| rtp 5.0;value=5.0});];
    Array.ofList windowed |> should equal expectedvals 

[<Test>]
let ``windowing by width from start looking forward on a single interval should produce accurate results`` () =
    let rt = DateTimeOffset.FromUnixTimeSeconds <| int64 1531525039
    let rtp mins = rt + TimeSpan.FromMinutes mins
    let inpvals = [
        LinearFloatInterval ({position=Inclusive <| rtp 1.0;value=1.0},{position=Inclusive <| rtp 5.0;value=5.0});]; 
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let windowing = Tumbling (Start,LookingForward,TimeSpan.FromMinutes 1.0)
    let windowed = List.collect <| (fun s -> s.intvalues) <| Sequence.window TimePosition windowing inpseq
    let expectedvals = [
        LinearFloatInterval ({position=Inclusive <| rtp 1.0;value=1.0},{position=Exclusive <| rtp 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive <| rtp 2.0;value=2.0},{position=Exclusive <| rtp 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive <| rtp 3.0;value=3.0},{position=Exclusive <| rtp 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive <| rtp 4.0;value=4.0},{position=Exclusive <| rtp 5.0;value=5.0});];
    Array.ofList windowed |> should equal expectedvals 

[<Test>]
let ``aggregation windowed by width from start looking forward on a single interval should produce accurate results`` () =
    let rt = DateTimeOffset.FromUnixTimeSeconds <| int64 1531525039
    let rtp mins = rt + TimeSpan.FromMinutes mins
    let inpvals = [
        LinearFloatInterval ({position=Inclusive <| rtp 1.0;value=1.0},{position=Exclusive <| rtp 2.0;value=2.0});
        LinearFloatInterval ({position=Inclusive <| rtp 2.0;value=2.0},{position=Exclusive <| rtp 3.0;value=3.0});
        LinearFloatInterval ({position=Inclusive <| rtp 3.0;value=3.0},{position=Exclusive <| rtp 4.0;value=4.0});
        LinearFloatInterval ({position=Inclusive <| rtp 4.0;value=4.0},{position=Inclusive <| rtp 5.0;value=5.0});]; 
    let inpseq = {id="test"; intvalues=inpvals; preextrap=NoExtrapolation();postextrap=NoExtrapolation() }
    let windowing = Tumbling (Start,LookingForward,TimeSpan.FromMinutes 1.0)
    let windowed = Sequence.window TimePosition windowing inpseq
    let agged = List.collect (fun s -> FloatOps.Mean OnTimePosition s) windowed
    let expectedvals = [|
        ConstantFloatInterval (Inclusive <| rtp 1.0, Exclusive <| rtp 2.0) 1.5;
        ConstantFloatInterval (Inclusive <| rtp 2.0, Exclusive <| rtp 3.0) 2.5;
        ConstantFloatInterval (Inclusive <| rtp 3.0, Exclusive <| rtp 4.0) 3.5;
        ConstantFloatInterval (Inclusive <| rtp 4.0, Exclusive <| rtp 5.0) 4.5;|]; 
    Array.ofList agged |> should equal expectedvals