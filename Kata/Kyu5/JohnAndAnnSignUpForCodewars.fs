namespace Kata.Kyu5

// https://www.codewars.com/kata/john-and-ann-sign-up-for-codewars

open FsUnit
open NUnit.Framework

module JohnAndAnnSignUpForCodewars =
    let johns = fst
    let anns = snd
    let katasCountSeq mapping n =
        Seq.unfold (fun (i, (john: list<int>), (ann: list<int>)) -> 
            let rec john' n' =
                match n' with
                | 0 -> 0
                | _ -> n' - ann'(john.[i-n'])
            and ann' n' =
                match n' with
                | 0 -> 1
                | _ -> n' - john'(ann.[i-n'])
            if i <> n then 
                let j = john'(i)
                let a = ann'(i)
                Some((j, a), (i+1, j::john, a::ann)) 
            else 
                None
            ) (0, [], [])
        |> Seq.map mapping
        |> Seq.toList
    
    let john = katasCountSeq johns
    let ann = katasCountSeq anns
    let sumJohn = john >> Seq.sum
    let sumAnn = ann >> Seq.sum

    [<Test>]
    let ``John and Ann sign up for Codewars`` () =
        let ``assert`` f n expected = f n |> should equal expected
        ``assert`` ann 6 [1; 1; 2; 2; 3; 3]
        ``assert`` ann 15 [1; 1; 2; 2; 3; 3; 4; 5; 5; 6; 6; 7; 8; 8; 9]
        ``assert`` john 11 [0; 0; 1; 2; 2; 3; 4; 4; 5; 6; 6]
        ``assert`` john 14 [0; 0; 1; 2; 2; 3; 4; 4; 5; 6; 6; 7; 7; 8]
        ``assert`` sumJohn 75 1720
        ``assert`` sumJohn 78 1861
        ``assert`` sumAnn 115 4070
        ``assert`` sumAnn 150 6930
