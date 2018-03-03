namespace Tests

open NUnit.Framework
open Monkey.Object

module Object_Test =

    [<Test>]
    let testStringHashKey() =
        let hello1 = {String.Value = "Hello World"} :> Hashable
        let hello2 = {String.Value = "Hello World"} :> Hashable
        let diff1 = {String.Value = "My name is johnny"} :> Hashable
        let diff2 = {String.Value = "My name is johnny"} :> Hashable

        if hello1.HashKey() <> hello2.HashKey() then
            Assert.Fail("Strings with same content have different hash keys")
        else if diff1.HashKey() <> diff2.HashKey() then
            Assert.Fail("Strings with same content have different hash keys")
        else if hello1.HashKey() = diff1.HashKey() then
            Assert.Fail("Strings with different content have same hash keys")
        else
            Assert.Pass("Hash keys work")
