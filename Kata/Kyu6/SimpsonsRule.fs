namespace Kata.Kyu6

// https://www.codewars.com/kata/simpsons-rule-approximate-integration

open FsUnit
open NUnit.Framework

module SimpsonsRule =
    let simpson (n: int) = 
        let a = 0.0
        let b = System.Math.PI
        let h = (b-a) / float(n)
        let f x = (3.0/2.0 * float(sin(x)**3.0))
        ((b-a) / (3.0 * float(n))) * 
            (f(a) 
            + f(b) 
            + 4.0 * ([1..n/2] |> List.sumBy (fun i -> f(a + (2.0*float(i) - 1.0) * h))) 
            + 2.0 * ([1..(n/2)-1] |> List.sumBy (fun i -> f(a + 2.0*float(i) * h))))

    [<Test>]
    let ``Simpson's Rule - Approximate Integration`` () =
        let assertFuzzyEquals actual expected =
            actual |> should (equalWithin (abs(actual - expected) <= 1e-10)) expected

        assertFuzzyEquals (simpson 290) 1.9999999986
        assertFuzzyEquals (simpson 72) 1.9999996367
        assertFuzzyEquals (simpson 252) 1.9999999975
        assertFuzzyEquals (simpson 40) 1.9999961668
        assertFuzzyEquals (simpson 276) 1.9999999983
        assertFuzzyEquals (simpson 384) 1.9999999995
        assertFuzzyEquals (simpson 30) 1.9999878155
        assertFuzzyEquals (simpson 238) 1.9999999969
        assertFuzzyEquals (simpson 20) 1.9999372878
