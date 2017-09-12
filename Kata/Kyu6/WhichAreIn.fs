namespace Kata.Kyu6

// https://www.codewars.com/kata/which-are-in

open FsUnit
open NUnit.Framework

module WhichAreIn =
    let inArray (a1: string list) (a2: string list) =
        a1 
        |> List.filter (fun a1' -> a2 |> List.exists (fun a2' -> a2'.Contains(a1')))
        |> List.distinct
        |> List.sort

    [<Test>]
    let ``Which are in?`` () =
        let assertInArray a1 a2 expected =
            inArray a1 a2 |> should equal expected

        assertInArray ["arp"; "live"; "strong"] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] ["arp"; "live"; "strong"]
        assertInArray ["xyz"; "live"; "strong"] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] ["live"; "strong"]
        assertInArray ["live"; "strong"; "arp"] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] ["arp"; "live"; "strong"]
        assertInArray ["live"; "strong"; "lyal"; "lysh"; "arp"] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] ["arp"; "live"; "strong"]
        assertInArray ["tarp"; "mice"; "bull"] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] []
        assertInArray [] ["lively"; "alive"; "harp"; "sharp"; "armstrong"] []
        assertInArray ["duplicates"; "duplicates"] ["duplicates"; "duplicates"] ["duplicates"]
