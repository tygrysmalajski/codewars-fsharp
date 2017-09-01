namespace Kata.Kyu6

// https://www.codewars.com/kata/disease-spread

open NUnit.Framework
open Kata.Asserts

module DiseaseSpread =
    let simulate tm n s0 i0 b a =
        let tm' = float(tm)
        let dt = tm' / float(n)
        Seq.unfold (fun (t, s, i, r) -> 
            if t <= tm' then
                let x = (
                    t + dt,
                    s - dt * b * s * i, 
                    i + dt * (b * s * i - a * i), 
                    r + dt * i * a)
                Some((t, s, i, r), x)
            else None
        ) (0.0, s0, i0, 0.0)

    let epidemic tm n s0 i0 b a =
        let Infected (_, _, i, _) = i
        simulate tm n s0 i0 b a
        |> Seq.maxBy Infected
        |> (Infected >> int)

    [<Test>]
    let ``Disease Spread`` () =
        assertFuzzyEquals (epidemic 18 432 1004.0 1.0 0.00209 0.51) 420
        assertFuzzyEquals (epidemic 12 288 1007.0 2.0 0.00206 0.45) 461
        assertFuzzyEquals (epidemic 13 312 999.0 1.0 0.00221 0.55) 409
        assertFuzzyEquals (epidemic 24 576 1005.0 1.0 0.00216 0.45) 474
        assertFuzzyEquals (epidemic 24 576 982.0 1.0 0.00214 0.44) 460
        assertFuzzyEquals (epidemic 20 480 1000.0 1.0 0.00199 0.53) 386
        assertFuzzyEquals (epidemic 28 672 980.0 1.0 0.00198 0.44) 433
        assertFuzzyEquals (epidemic 14 336 996.0 2.0 0.00206 0.41) 483
        assertFuzzyEquals (epidemic 13 312 993.0 2.0 0.0021 0.51) 414
        assertFuzzyEquals (epidemic 28 672 999.0 1.0 0.00197 0.55) 368
