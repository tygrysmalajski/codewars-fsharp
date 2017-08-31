namespace Kata.Kyu6

// https://www.codewars.com/kata/are-they-the-same

module AreTheyTheSame =

    open NUnit.Framework
    open FsUnit

    let comp (xs: list<int>, ys: list<int>) = 
        if xs.Length <> ys.Length then 
            false
        else
            match (xs, ys) with
            | [], _ -> true
            | _, [] -> true
            | xs, ys -> 
                let sort = List.map abs >> List.sort
                let xs' = sort xs
                let ys' = sort ys
                List.zip xs' ys' |> List.forall (fun (x, y) -> x * x = y)

    [<Test>]
    let ``Are they the "same"?`` () =
        comp ([], []) |> should be True

        let a1 = [121; 144; 19; 161; 19; 144; 19; 11]
        let a2 = [11*11; 121*121; 144*144; 19*19; 161*161; 19*19; 144*144; 19*19]
        comp (a1, a2) |> should be True

        let a3 = [121; 144; 19; 161; 19; 144; 19; 11]
        let a4 = [11*21; 121*121; 144*144; 19*19; 161*161; 19*19; 144*144; 19*19]
        comp (a3, a4) |> should be False

        let a4 = [121; 144; 19; 161; 19; 144; 19; 11]
        let a5 = [11*11; 121*121; 144*144; 190*190; 161*161; 19*19; 144*144; 19*19]
        comp (a4, a5) |> should be False

        let a6 = [121; 144; 19; 161; 19; 144; 19; 11; 1008]
        let a7 = [11*11; 121*121; 144*144; 190*190; 161*161; 19*19; 144*144; 19*19]
        comp (a6, a7) |> should be False

        let a8 = [10000000; 100000000]
        let a9 = [10000000 * 10000000; 100000000 * 100000000]
        comp (a8, a9) |> should be True

        let a10 = [10000001; 100000000]
        let a11 = [10000000 * 10000000; 100000000 * 100000000]
        comp (a10, a11) |> should be False

        let a12 = [2; 2; 3]
        let a13 = [4; 9; 9]
        comp (a12, a13) |> should be False

        let a14 = [17; -16; 5; 3; -2; 28; 2; -15; -11; 20; 1; -16; 14; 4; -13; -13; -12; -8; 10; 18; 18; 11; 25]
        let a15 = [289; 256; 25; 9; 4; 784; 4; 225; 121; 400; 1; 256; 196; 16; 169; 169; 144; 64; 100; 324; 324; 121; 625]
        comp (a14, a15) |> should be True
