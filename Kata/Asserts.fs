namespace Kata

open FsUnit

module Asserts =
    let assertFuzzyEquals actual expected =
        actual |> should (equalWithin 1e-10) expected
