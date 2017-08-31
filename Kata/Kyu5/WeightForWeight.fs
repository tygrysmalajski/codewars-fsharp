namespace Kata.Kyu5

// https://www.codewars.com/kata/weight-for-weight

open FsUnit
open NUnit.Framework

module WeightForWeight =
    let orderWeight (s: string) = 
        let convert = Seq.sumBy (System.Char.GetNumericValue >> int)
        s.Split([|' '|])
        |> Seq.map (fun w -> (convert w, w))
        |> Seq.sort
        |> Seq.map snd
        |> String.concat " "

    [<Test>]
    let ``Weight for weight`` () =
        let assertOrderWeight s expected =
            orderWeight s |> should equal expected

        assertOrderWeight "103 123 4444 99 2000" "2000 103 123 4444 99"
        assertOrderWeight "2000 10003 1234000 44444444 9999 11 11 22 123" "11 11 2000 10003 22 123 1234000 44444444 9999"
        assertOrderWeight "" ""
        assertOrderWeight "10003 1234000 44444444 9999 2000 123456789" "2000 10003 1234000 44444444 9999 123456789"
        assertOrderWeight "3 16 9 38 95 1131268 49455 347464" "3 16 9 38 95 1131268 49455 347464"
        assertOrderWeight "71899703 200 6 91 425 4 67407 7 96488 6 4 2 7 31064 9 7920 1 34608557 27 72 18 81" 
            "1 2 200 4 4 6 6 7 7 18 27 72 81 9 91 425 31064 7920 67407 96488 34608557 71899703"
        assertOrderWeight "387087 176 351832 100 430372 8 58052 54 175432 120 269974 147 309754 91 404858 67 271476 164 295747 111 40"
            "100 111 120 40 8 54 91 164 147 67 176 430372 58052 175432 351832 271476 309754 404858 387087 295747 269974"
