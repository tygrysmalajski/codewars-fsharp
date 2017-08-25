namespace Kata.Kyu6

// https://www.codewars.com/kata/moves-in-squared-strings-iii

open NUnit.Framework
open Kata.MovesInSquaredStrings

module MovesInSquaredStringsIII =
    let private zipMap f1 f2 (ss: seq<string>) = 
        Seq.zip (f1 ss) (f2 ss)
        |> Seq.map (fun (l, r) -> l + "|" + r)

    let private transpose ss = 
        let rec transpose' acc = function
            | s::_ when System.String.IsNullOrWhiteSpace s -> List.rev acc
            | ss -> transpose' ((List.map (mapString2 Array.head) ss)::acc) (List.map (mapString Array.tail) ss)
        transpose' [] (ss |> Seq.toList)
        |> Seq.map (Seq.ofList)

    let private transposeMap mapping = transpose >> (Seq.map mapping)

    let diag1Sym = transposeMap (Seq.reduce (+))

    let rot90Clock ss = ss |> transposeMap (Seq.rev >> Seq.reduce(+))
                
    let selfieAndDiag1 ss = ss |> zipMap id diag1Sym

    [<Test>]
    let ``Moves in squared strings I`` () =
        let assertDiag1Sym = assertOper diag1Sym
        let assertRot90Clock = assertOper rot90Clock
        let assertSelfieAndDiag1 = assertOper selfieAndDiag1

        assertDiag1Sym "wuUyPC\neNHWxw\nehifmi\ntBTlFI\nvWNpdv\nIFkGjZ"
            "weetvI\nuNhBWF\nUHiTNk\nyWflpG\nPxmFdj\nCwiIvZ"

        assertRot90Clock "rgavce\nvGcEKl\ndChZVW\nxNWgXR\niJBYDO\nSdmEKb"
            "Sixdvr\ndJNCGg\nmBWhca\nEYgZEv\nKDXVKc\nbORWle"

        assertSelfieAndDiag1 "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"
            "NJVGhr|NMtsrz\nMObsvw|JOPotj\ntPhCtl|VbhEQl\nsoEnhi|GsCnRi\nrtQRLK|hvthLW\nzjliWg|rwliKg"