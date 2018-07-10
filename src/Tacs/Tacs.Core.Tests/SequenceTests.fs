module SequenceTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types
open Tacs.Core.Sequence
open Tacs.Core
open Tacs.Core.IntegerOps
open Tacs.Core.FloatOps

[<Test>]
let ``points to intervals should be reversible`` () =
    let ptvals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let ptseq = {id="test";extrap=BeforeAndAfter;bound=InclusiveLow;ptvalues=ptvals}
    let intseq = remodelToLinearIntervals IntegerPosition LinearNearestIntValue ptseq
    let ptseq2 = remodelToPoints IntegerPosition IntervalStart InclusiveLow intseq
    ptseq2.ptvalues |> should equal ptseq.ptvalues

[<Test>]
let ``intervals to points should be reversible`` () =
    let intvals = [
        BackwardRayInterval {``end``=Exclusive 1;value=ConstantValue 1}; //still need to look at clamping-to-interval-domain
        LinearNearestIntInterval ({position=Inclusive 1;value=1},{position=Exclusive 2;value=2})
        LinearNearestIntInterval ({position=Inclusive 2;value=2},{position=Exclusive 3;value=3})
        LinearNearestIntInterval ({position=Inclusive 3;value=3},{position=Exclusive 4;value=4})
        LinearNearestIntInterval ({position=Inclusive 4;value=4},{position=Exclusive 5;value=5})
        ForwardRayInterval {start=Inclusive 5;value=ConstantValue 5}];
    let intseq = {id="test"; intvalues=intvals}
    let ptseq = remodelToPoints IntegerPosition IntervalStart InclusiveLow intseq
    let intseq2 = remodelToLinearIntervals IntegerPosition LinearNearestIntValue ptseq
    intseq2.intvalues |> should equal intseq.intvalues

[<Test>]
let ``linear interpolation should produce correct value for int positions and float values`` () =
    let inter = FloatOps.LinearFloatInterval ({position=Inclusive 1;value=1.0},{position=Exclusive 100;value=100.0})
    let pv = inter.value.At IntegerPosition 42
    pv |> should equal 42.0

[<Test>]
let ``linear interpolation should produce correct value for int positions and int values`` () =
    let inter = LinearNearestIntInterval ({position=Inclusive 1;value=1},{position=Exclusive 100;value=100})
    let pv = inter.value.At IntegerPosition 42
    pv |> should equal 42

[<Test>]
let ``linear interpolation should produce correct value for float positions and float values`` () =
    let inter = LinearFloatInterval ({position=Inclusive 1.0;value=1.0},{position=Exclusive 100.0;value=100.0})
    let pv = inter.value.At FloatPosition 42.0
    pv |> should equal 42.0

(*
[<Test>]
let ``interpolation should work in middle of sequences`` () =
    let intvals = [
        BackwardRayIntervalValue {``end``={position=1;value=1.0}};
        FiniteIntervalValue {start={position=1;value=1.0};``end``={position=2;value=2.0}};
        FiniteIntervalValue {start={position=2;value=2.0};``end``={position=5;value=5.0}};
        FiniteIntervalValue {start={position=5;value=5.0};``end``={position=6;value=6.0}};
        ForwardRayIntervalValue {start={position=6;value=6.0}}];
    let vstrat = Interpolate IntegerOps.InterpolatePosition FloatOps.InterpolateValueLinear
    let intseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=intvals}
    let iv = getPointInSequence intseq 4
    iv |> should equal (Some {position=4;value=4.0})

[<Test>]
let ``interpolation shouldn't find a value where there are none`` () =
    let intvals = [
        BackwardRayIntervalValue {``end``={position=1;value=1.0}};
        FiniteIntervalValue {start={position=1;value=1.0};``end``={position=2;value=2.0}};
        FiniteIntervalValue {start={position=2;value=2.0};``end``={position=3;value=3.0}};
        FiniteIntervalValue {start={position=5;value=5.0};``end``={position=6;value=6.0}};
        ForwardRayIntervalValue {start={position=6;value=6.0}}];
    let vstrat = Interpolate IntegerOps.InterpolatePosition FloatOps.InterpolateValueLinear
    let intseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=intvals}
    let iv = getPointInSequence intseq 4
    iv |> should equal None

[<Test>]
let ``interval slice on point locations should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear   
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = { IntervalSlice.start=Some {location=2.0;strategy=BoundaryStrategy.Interpolated};``end``= Some {location=4.0;strategy=BoundaryStrategy.Interpolated}}   
    let subseq = sliceByInterval slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``interval slice between point locations should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear 
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = { IntervalSlice.start=Some {location=1.5;strategy=BoundaryStrategy.Interpolated};``end``= Some {location=3.5;strategy=BoundaryStrategy.Interpolated}}   
    let subseq = sliceByInterval slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=1.5;value=1.5}};
        FiniteIntervalValue {start={position=1.5;value=1.5};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=3.5;value=3.5}};
        ForwardRayIntervalValue {start={position=3.5;value=3.5}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``unbounded-end interval slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear   
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = { IntervalSlice.start=Some {location=3.5;strategy=BoundaryStrategy.Interpolated};``end``= None}   
    let subseq = sliceByInterval slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=3.5;value=3.5}};
        FiniteIntervalValue {start={position=3.5;value=3.5};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward interpolated slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear 
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {ForwardSlice.start={location=1.5;strategy=BoundaryStrategy.Interpolated};count=3}   
    let subseq = sliceForwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=1.5;value=1.5}};
        FiniteIntervalValue {start={position=1.5;value=1.5};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward intersected slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear  
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {ForwardSlice.start={location=1.5;strategy=BoundaryStrategy.Intersected};count=3}   
    let subseq = sliceForwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``forward inside slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear 
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {ForwardSlice.start={location=1.5;strategy=BoundaryStrategy.Inside};count=2}   
    let subseq = sliceForwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``overflowing forward inside slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear    
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {ForwardSlice.start={location=0.5;strategy=BoundaryStrategy.Inside};count=200}   
    let subseq = sliceForwardByCount slstrat fullseq
    let expectedvals = allvals
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)

[<Test>]
let ``backward interpolated slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear    
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {BackwardSlice.``end``={location=3.5;strategy=BoundaryStrategy.Interpolated};count=2}   
    let subseq = sliceBackwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=3.5;value=3.5}};
        ForwardRayIntervalValue {start={position=3.5;value=3.5}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``backward intersected slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear    
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {BackwardSlice.``end``={location=3.5;strategy=BoundaryStrategy.Intersected};count=2}   
    let subseq = sliceBackwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``backward inside slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear    
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {BackwardSlice.``end``={location=4.5;strategy=BoundaryStrategy.Inside};count=2}   
    let subseq = sliceBackwardByCount slstrat fullseq
    let expectedvals = [
        BackwardRayIntervalValue {``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        ForwardRayIntervalValue {start={position=4.0;value=4.0}}];
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals)  

[<Test>]
let ``overflowing backward inside slice should produce correct subsequence`` () =
    let allvals = [
        BackwardRayIntervalValue {``end``={position=1.0;value=1.0}};
        FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=2.0;value=2.0}};
        FiniteIntervalValue {start={position=2.0;value=2.0};``end``={position=3.0;value=3.0}};
        FiniteIntervalValue {start={position=3.0;value=3.0};``end``={position=4.0;value=4.0}};
        FiniteIntervalValue {start={position=4.0;value=4.0};``end``={position=5.0;value=5.0}};
        ForwardRayIntervalValue {start={position=5.0;value=5.0}}];
    let vstrat = Interpolate FloatOps.InterpolatePosition FloatOps.InterpolateValueLinear    
    let fullseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=allvals}
    let slstrat = {BackwardSlice.``end``={location=5.5;strategy=BoundaryStrategy.Inside};count=200}   
    let subseq = sliceBackwardByCount slstrat fullseq
    let expectedvals = allvals
    Array.ofList subseq.intvalues |> should equal (Array.ofList expectedvals) *)