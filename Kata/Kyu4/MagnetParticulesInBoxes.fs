namespace Kata.Kyu4

// https://www.codewars.com/kata/magnet-particules-in-boxes

open NUnit.Framework
open Kata.Asserts

module MagnetParticulesInBoxes =
    let doubles maxk maxn = 
        let v k n = 1.0 / float(((float(k)) * (pown (float(n)+1.0) (2*k))))
        [1..maxk] |> List.sumBy (fun k -> [1..maxn] |> List.sumBy (fun n -> v k n))

    [<Test>]
    let ``Magnet particules in boxes`` () =
        assertFuzzyEquals (doubles 1 10) 0.5580321939764581
        assertFuzzyEquals (doubles 10 1000) 0.6921486500921933
        assertFuzzyEquals (doubles 15 1000) 0.6921486782113179
        assertFuzzyEquals (doubles 20 1500) 0.6924811798693108
        assertFuzzyEquals (doubles 30 2000) 0.6926475552685089
        assertFuzzyEquals (doubles 90 10000) 0.693047195557606
        assertFuzzyEquals (doubles 10 100) 0.6832948559787737
        assertFuzzyEquals (doubles 25 12000) 0.6930638576419238
        assertFuzzyEquals (doubles 40 15000) 0.6930805205592468
