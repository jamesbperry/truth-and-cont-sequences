module SequenceTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types
open Tacs.Core.Sequence
open Tacs.Core.Numerics

[<Test>]
let ``points to intervals should be reversible`` () =
    let ptvals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let pstrat = IntPosition()
    let vstrat = IntValueStrategy(Linear, pstrat)
    let ptseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=vstrat;ptvalues=ptvals}
    let intseq = remodelToIntervals ptseq
    let ptseq2 = remodelToPoints RemodelAnchor.IntervalStart intseq
    ptseq2 |> should equal ptseq

[<Test>]
let ``linear interpolation should produce correct value for int positions and float values`` () =
    let iv = FiniteIntervalValue {start={position=1;value=1.0};``end``={position=100;value=100.0}}
    let pos = 42
    let pstrat = IntPosition()
    let vstrat = FloatValueStrategy(Linear,pstrat)
    let pv = getValue vstrat iv pos
    pv |> should equal {position=42;value=42.0}

[<Test>]
let ``linear interpolation should produce correct value for int positions and int values`` () =
    let iv = FiniteIntervalValue {start={position=1;value=1};``end``={position=100;value=100}}
    let pos = 42
    let pstrat = IntPosition()
    let vstrat = IntValueStrategy(Linear,pstrat)
    let pv = getValue vstrat iv pos
    pv |> should equal {position=42;value=42}

[<Test>]
let ``linear interpolation should produce correct value for float positions and float values`` () =
    let iv = FiniteIntervalValue {start={position=1.0;value=1.0};``end``={position=100.0;value=100.0}}
    let pos = 42.42
    let pstrat = FloatPosition()
    let vstrat = FloatValueStrategy(Linear,pstrat)
    let pv = getValue vstrat iv pos
    pv |> should equal {position=42.42;value=42.42}