module DummyTests

open NUnit.Framework
open FsUnit

[<Test>]
let ``one should equal one`` () =
    1 |> should equal 1