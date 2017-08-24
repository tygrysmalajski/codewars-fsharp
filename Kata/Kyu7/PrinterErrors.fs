namespace Kata.Kyu7

module PrinterErrors =

    open NUnit.Framework
    open FsUnit

    let printerError(s: string) =
        let colors = seq['a'..'m']
        let isError c = colors |> Seq.exists((=) c) |> not
        let errors = s |> Seq.filter isError |> Seq.length
        sprintf "%i/%i" errors s.Length

    [<Test>]
    let ``Printer Errors`` () =
        let assertErrors controlString errorRate =
            controlString
            |> printerError
            |> should equal errorRate

        "aaabbbbhaijjjm" |> assertErrors <| "0/14"
        "aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyz"
        |> assertErrors <| "3/56"
        "kkkwwwaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyz"
        |> assertErrors <| "6/60"
        "kkkwwwaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbmmmmmmmmmmmmmmmmmmmxyzuuuuu"
        |> assertErrors <| "11/65"