module DummyTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types

[<Test>]
let ``one should equal one`` () =
    1 |> should equal 1

[<Test>]
let ``identical PointValues should resolve as equal`` () =
    {position=1;value=42} |> should equal {position=1;value=42}