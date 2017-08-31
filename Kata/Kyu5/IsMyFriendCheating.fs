namespace Kata.Kyu5

// https://www.codewars.com/kata/is-my-friend-cheating

open FsUnit
open NUnit.Framework

module IsMyFriendCheating =
    let removNb (n: int64) =
        let sum = n * (n + 1L) / 2L
        seq {
            for a in [n/2L..n] do
                let (b, r) = System.Math.DivRem(sum-a, a+1L)
                if r = 0L && a <> b then
                    yield (a, b)
        } |> Seq.toList

    [<Test>]
    let ``Is my friend cheating?`` () =
        let assertRemovNb n (expected: list<int64 * int64>) =
            removNb n |> should equal expected

        assertRemovNb 26L [(15L,21L);(21L,15L)]
        assertRemovNb 100L []
        assertRemovNb 103L []
        assertRemovNb 200L []
        assertRemovNb 37L [(21L,31L);(31L,21L)]
        assertRemovNb 101L [(55L,91L);(91L,55L)]
        assertRemovNb 102L [(70L,73L);(73L,70L)]
        assertRemovNb 110L [(70L,85L);(85L,70L)]
        assertRemovNb 210L  [(115L,190L);(190L,115L)]
        assertRemovNb 213L [(147L,153L);(153L,147L)]

        assertRemovNb 300L [(162L,276L);(276L,162L)]
        assertRemovNb 311L [(202L,238L);(238L,202L)]
        assertRemovNb 369L [(213L,318L);(318L,213L)]
        assertRemovNb 394L [(273L,283L);(283L,273L)]
        assertRemovNb 341L [(196L,295L);(295L,196L)]
        assertRemovNb 378L [(241L,295L);(295L,241L)]   
        assertRemovNb 446L [(252L,393L);(393L,252L)]
        assertRemovNb 846L [(498L,717L);(717L,498L)]
        assertRemovNb 906L [(505L,811L);(615L,666L);(637L,643L);(643L,637L);(666L,615L);(811L,505L)]
        assertRemovNb 1922L [(1218L,1515L);(1515L,1218L)]  

        assertRemovNb 1000008L [(677076L,738480L);(738480L,677076L)]
        assertRemovNb 1000003L [(550320L,908566L);(559756L,893250L);(893250L,559756L);(908566L,550320L)]
        assertRemovNb 1000007L []       
