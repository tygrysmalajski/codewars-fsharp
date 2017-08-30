namespace Kata.Kyu6

// https://www.codewars.com/kata/take-a-ten-minute-walk

open FsUnit
open NUnit.Framework

module TakeATenMinuteWalk =
    let isValidWalk =
        List.fold (fun (i, n, s, e, w) c -> 
            match c with
            | _ when c = 'n' -> (i+1, n+1, s, e, w)
            | _ when c = 's' -> (i+1, n, s+1, e, w)
            | _ when c = 'e' -> (i+1, n, s, e+1, w)
            | _ when c = 'w' -> (i+1, n, s, e, w+1)
            | _ -> failwith "Invalid direction"
        ) (0, 0, 0, 0, 0)
        >> (fun (i, n, s, e, w) -> i = 10 && n = s && e = w)

    [<Test>]
    let ``Take a Ten Minute Walk`` () =
        let assertWalk walk expected =
            isValidWalk walk |> should equal expected

        assertWalk ['n';'s';'n';'s';'n';'s';'n';'s';'n';'s'] true
        assertWalk ['n';'s';'n';'s';'n';'s';'n';'s';'n';'n'] false
        assertWalk ['n';'s'] false
        assertWalk ['n';'s';'e';'w';'n';'s';'e';'w';'n';'s'] true
