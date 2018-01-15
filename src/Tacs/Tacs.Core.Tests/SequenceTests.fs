module SequenceTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types
open Tacs.Core.Sequence
open Tacs.Core.Numerics

[<Test>]
let ``points to intervals should be reversible`` () =
    let ptvals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let vstrat = InterpolateInt Linear IntPositionScale
    let ptseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;ptvalues=ptvals}
    let intseq = remodelToIntervals ptseq
    let ptseq2 = remodelToPoints RemodelAnchor.IntervalStart intseq
    ptseq2.ptvalues |> should equal ptseq.ptvalues

[<Test>]
let ``intervals to points should be reversible`` () =
    let intvals = [
        BackwardRayIntervalValue {``end``={position=1;value=1}};
        FiniteIntervalValue {start={position=1;value=1};``end``={position=2;value=2}};
        FiniteIntervalValue {start={position=2;value=2};``end``={position=3;value=3}};
        FiniteIntervalValue {start={position=3;value=3};``end``={position=4;value=4}};
        ForwardRayIntervalValue {start={position=4;value=4}}];
    let vstrat = InterpolateInt Linear IntPositionScale
    let intseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=intvals}
    let ptseq = remodelToPoints RemodelAnchor.IntervalStart intseq
    let intseq2 = remodelToIntervals ptseq
    intseq2.intvalues |> should equal intseq.intvalues

[<Test>]
let ``linear interpolation should produce correct value for int positions and float values`` () =
    let iv = FiniteIntervalValue {start={position=1;value=1.0};``end``={position=100;value=100.0}}
    let pos = 42
    let vstrat = InterpolateFloat Linear IntPositionScale
    let pv = getPointInInterval vstrat (Some iv) pos
    pv |> should equal (Some {position=42;value=42.0})

[<Test>]
let ``linear interpolation should produce correct value for int positions and int values`` () =
    let iv = FiniteIntervalValue {start={position=1;value=1};``end``={position=100;value=100}}
    let pos = 42
    let vstrat = InterpolateInt Linear IntPositionScale
    let pv = getPointInInterval vstrat (Some iv) pos
    pv |> should equal (Some {position=42;value=42})

[<Test>]
let ``linear interpolation should produce correct value for float positions and float values`` () =
    let iv = FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=100.0;value=100.0}}
    let pos = 42.42
    let vstrat = InterpolateFloat Linear FloatPositionScale
    let pv = getPointInInterval vstrat (Some iv) pos
    pv |> should equal (Some {position=42.42;value=42.42})

[<Test>]
let ``interpolation should work in middle of sequences`` () =
    let intvals = [
        BackwardRayIntervalValue {``end``={position=1;value=1.0}};
        FiniteIntervalValue {start={position=1;value=1.0};``end``={position=2;value=2.0}};
        FiniteIntervalValue {start={position=2;value=2.0};``end``={position=5;value=5.0}};
        FiniteIntervalValue {start={position=5;value=5.0};``end``={position=6;value=6.0}};
        ForwardRayIntervalValue {start={position=6;value=6.0}}];
    let vstrat = InterpolateFloat Linear IntPositionScale
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
    let vstrat = InterpolateFloat Linear IntPositionScale
    let intseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;intvalues=intvals}
    let iv = getPointInSequence intseq 4
    iv |> should equal None