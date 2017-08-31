namespace Kata.Kyu4

// https://www.codewars.com/kata/twice-linear

open FsUnit
open NUnit.Framework
open System.Collections.Generic

module TwiceLinear =
    let add (xs: SortedSet<'a>) = xs.Add >> ignore
    let rem (xs: SortedSet<'a>) = xs.Remove >> ignore
    let sortedSet xs = 
        let set = new SortedSet<'a>()
        xs |> Seq.map (add set) |> ignore
        set

    let dblLinear n =
        Seq.unfold (fun (i, (xs: SortedSet<int>)) -> 
            if i <= n+1 then
                let x = xs.Min
                add xs (2*x+1)
                add xs (3*x+1)
                rem xs (x)
                Some(x, (i+1, xs))
            else
                None)
            (0, sortedSet [1])
        |> Seq.last

    [<Test>]
    let ``Twice linear`` () =
        let assertDblLinear x expected =
            dblLinear x |> should equal expected

        assertDblLinear 10 22
        assertDblLinear 20 57
        assertDblLinear 30 91
        assertDblLinear 50 175
        assertDblLinear 100 447
        assertDblLinear 500 3355
        assertDblLinear 1000 8488
        assertDblLinear 2000 19773
        assertDblLinear 6000 80914
        assertDblLinear 60000 1511311
