namespace Kata.Kyu6

// https://www.codewars.com/kata/fibonacci-tribonacci-and-friends

open NUnit.Framework
open FsUnit

module FibonacciTribonacciAndFriends =
    let xbonacci signature n = 
        (signature |> Seq.append <| Seq.unfold
        (fun xs -> 
            match xs with
            | _::ys ->
                let x = List.sum xs
                Some(x, List.append ys (List.singleton x))
            | _ -> None
        ) signature)
        |> Seq.truncate n
        |> Seq.toList
    
    [<Test>]
    let ``Fibonacci, Tribonacci and friends`` () =
        xbonacci [0;1] 10 |> should equal [0;1;1;2;3;5;8;13;21;34]
        xbonacci [1;1] 10 |> should equal [1;1;2;3;5;8;13;21;34;55]
        xbonacci [0;0;0;0;1] 10 |> should equal [0;0;0;0;1;1;2;4;8;16]
        xbonacci [0;0;0;0;1] 30 |> should equal [0; 0; 0; 0; 1; 1; 2; 4; 8; 16; 31; 61; 120; 236; 464; 912; 1793; 3525; 6930;13624; 26784; 52656; 103519; 203513; 400096; 786568; 1546352; 3040048;5976577; 11749641]
        xbonacci [1;0;0;0;0;0;1] 10 |> should equal [1;0;0;0;0;0;1;2;3;6]
        xbonacci [1;0;0;0;0;0;0;0;0;0] 20 |> should equal [1;0;0;0;0;0;0;0;0;0;1;1;2;4;8;16;32;64;128;256]
        xbonacci [0;0;0;0;0;0;0;0;0;0] 20 |> should equal [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]
        xbonacci [1;2;3;4;5;6;7;8;9;0] 9 |> should equal [1;2;3;4;5;6;7;8;9]
        xbonacci [1;2;3;4;5;6;7;8;9;0] 0 |> should equal []
        xbonacci [1;2;3;5;7;9;11;13;17;19;23] 20 |> should equal [1; 2; 3; 5; 7; 9; 11; 13; 17; 19; 23; 110; 219; 436; 869; 1733; 3459; 6909; 13807; 27601]
