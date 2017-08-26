namespace Kata.Kyu6

// https://www.codewars.com/kata/moves-in-squared-strings-iii

open NUnit.Framework
open Kata.Kyu7.MovesInSquaredStringsI

module MovesInSquaredStringsIII =
    let private zipN xs = xs |> (Seq.collect Seq.indexed >> Seq.groupBy fst >> Seq.map (snd >> Seq.map snd))
    
    let private zipMap fs sep (ss: seq<string>) = 
        fs 
        |> Seq.map (fun f -> f ss)
        |> zipN
        |> Seq.map (Seq.chunkBySize (Seq.length fs))
        |> Seq.collect (Seq.map (String.concat sep))

    let private transpose ss = 
        let rec transpose' acc = function
            | s::_ when System.String.IsNullOrWhiteSpace s -> List.rev acc
            | ss -> transpose' ((List.map (mapString2 Array.head) ss)::acc) (List.map (mapString Array.tail) ss)
        transpose' [] (ss |> Seq.toList)
        |> Seq.map Seq.ofList

    let private transposeMap mapping = transpose >> (Seq.map mapping)

    let zipConcat fs = zipMap fs "|"
    let diag1Sym = transposeMap (Seq.reduce (+))
    let rot90Clock = transposeMap (Seq.rev >> Seq.reduce(+))
    let selfieAndDiag1 = zipConcat [id; diag1Sym]

    [<Test>]
    let ``Moves in squared strings (III)`` () =
        let assertDiag1Sym = assertOper diag1Sym
        let assertRot90Clock = assertOper rot90Clock
        let assertSelfieAndDiag1 = assertOper selfieAndDiag1

        assertDiag1Sym "wuUyPC\neNHWxw\nehifmi\ntBTlFI\nvWNpdv\nIFkGjZ"
            "weetvI\nuNhBWF\nUHiTNk\nyWflpG\nPxmFdj\nCwiIvZ"
        assertDiag1Sym "qAdPMX\nkRIQKU\nJeoroo\nNwVbtn\nAmQUqi\nVguxub"
            "qkJNAV\nARewmg\ndIoVQu\nPQrbUx\nMKotqu\nXUonib"
        assertDiag1Sym "gBHG\nmjIc\nJgkG\nfPjL" "gmJf\nBjgP\nHIkj\nGcGL"

        assertRot90Clock "rgavce\nvGcEKl\ndChZVW\nxNWgXR\niJBYDO\nSdmEKb"
            "Sixdvr\ndJNCGg\nmBWhca\nEYgZEv\nKDXVKc\nbORWle"
        assertRot90Clock "EFAxSN\nXbJObC\nMrNVyg\nUKqDsE\nrYnAfU\nnNjADZ"
            "nrUMXE\nNYKrbF\njnqNJA\nAADVOx\nDfsybS\nZUEgCN"
        assertRot90Clock "RPusfa\nvxieXA\nEGNMDi\nWjYSQJ\nnpMqdK\nTYvcbx"
            "TnWEvR\nYpjGxP\nvMYNiu\ncqSMes\nbdQDXf\nxKJiAa"

        assertSelfieAndDiag1 "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"
            "NJVGhr|NMtsrz\nMObsvw|JOPotj\ntPhCtl|VbhEQl\nsoEnhi|GsCnRi\nrtQRLK|hvthLW\nzjliWg|rwliKg"
        assertSelfieAndDiag1 "JAAn\nsrpa\nFngg\nmrVJ" "JAAn|JsFm\nsrpa|Arnr\nFngg|ApgV\nmrVJ|nagJ"
        assertSelfieAndDiag1 "MpbD\nfxUT\ndUfr\nbtbs" "MpbD|Mfdb\nfxUT|pxUt\ndUfr|bUfb\nbtbs|DTrs"   