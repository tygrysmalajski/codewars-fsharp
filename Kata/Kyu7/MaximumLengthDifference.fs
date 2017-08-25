namespace Kata.Kyu7

// https://www.codewars.com/kata/5663f5305102699bad000056

module MaximumLengthDifference =

    open NUnit.Framework
    open FsUnit

    let maxLengthDiff (xs: string[]) (ys: string[]) =
        let minMaxLength (xs: string[]) =
            let lengths = xs |> Seq.map (fun x -> x.Length)
            let x1 = Seq.head lengths
            lengths |> Seq.fold (fun (min', max') x -> (min x min', max x max')) (x1, x1)
        match (xs, ys) with
        | [||], _ -> None
        | _, [||] -> None
        | xs, ys ->
            let (xMin, xMax) = xs |> minMaxLength
            let (yMin, yMax) = ys |> minMaxLength
            let (min, max) = if xMax > yMax then (yMin, xMax) else (xMin, yMax)
            abs max-min |> Some

    [<Test>]
    let ``Maximum Length Difference`` () =
        let assertMaxLengthDiff s1 s2 result =
            maxLengthDiff s1 s2 |> should equal result

        let s1 = [|"hoqq"; "bbllkw"; "oox"; "ejjuyyy"; "plmiis"; "xxxzgpsssa"; "xxwwkktt"; "znnnnfqknaz"; "qqquuhii"; "dvvvwz"|]
        let s2 = [|"cccooommaaqqoxii"; "gggqaffhhh"; "tttoowwwmmww"|]
        assertMaxLengthDiff s1 s2 (Some 13)
        let s3 = [||]
        let s4 = [|"cccooommaaqqoxii"; "gggqaffhhh"; "tttoowwwmmww"|]
        assertMaxLengthDiff s3 s4 None