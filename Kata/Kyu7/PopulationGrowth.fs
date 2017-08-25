namespace Kata.Kyu7

// https://www.codewars.com/kata/563b662a59afc2b5120000c6

module PopulationGrowth =

    open NUnit.Framework
    open FsUnit

    let nbYear (p0: int) (percent: float) (aug: int) (p: int) =
        let per = percent / 100.0
        let grow (population, years) = 
            let state = (population + int((float(population) * per)) + aug, years+1)
            Some(state, state)
        Seq.unfold grow (p0, 0)
        |> Seq.find (fun (population, _) -> population >= p)
        |> snd

    [<Test>]
    let ``Growth of a Population`` () =
        let assertGrowth p0 percent aug p expected =
            nbYear p0 percent aug p |> should equal expected

        assertGrowth 1500 5.0 100 5000 15
        assertGrowth 1500000 2.5 10000 2000000 10
        assertGrowth 1500000 0.25 1000 2000000 94