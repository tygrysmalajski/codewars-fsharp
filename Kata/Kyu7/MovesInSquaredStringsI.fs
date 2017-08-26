namespace Kata.Kyu7

// https://www.codewars.com/kata/moves-in-squared-strings-i

open FsUnit
open NUnit.Framework

module MovesInSquaredStringsI =
    let mapString (mapping: char[] -> char[]) (s: string) = new string((s.ToCharArray()) |> mapping)
    let mapString2 (mapping: char[] -> char) = mapString (mapping >> (fun x -> [|x|]))
    let apply (map: seq<string> -> seq<string>) =
        let split (s: string) = s.Split([||])
        let concat = String.concat "\n"
        split 
        >> map
        >> concat

    let oper fct s = fct s

    let assertOper oper s expected =
        s |> apply oper |> should equal expected

    let vertMirror = Seq.map (mapString Array.rev)
    let horMirror = Seq.rev

    [<Test>]
    let ``Moves in squared strings (I)`` () =
        let assertVertMirror = assertOper vertMirror
        let assertHorMirror = assertOper horMirror

        assertVertMirror "hSgdHQ\nHnDMao\nClNNxX\niRvxxH\nbqTVvA\nwvSyRu" "QHdgSh\noaMDnH\nXxNNlC\nHxxvRi\nAvVTqb\nuRySvw"
        assertVertMirror "IzOTWE\nkkbeCM\nWuzZxM\nvDddJw\njiJyHF\nPVHfSx" "EWTOzI\nMCebkk\nMxZzuW\nwJddDv\nFHyJij\nxSfHVP"
        assertVertMirror "cuQW\nxOuD\nfZwp\neqFx" "WQuc\nDuOx\npwZf\nxFqe"
        assertVertMirror "CDGIdolo\nUstXfrIg\ntMqjvxWL\nBEyuCnAm\nxsxaEERa\nxZsvOiZS\nLutlBRXE\ntxshhbqE"
            "olodIGDC\ngIrfXtsU\nLWxvjqMt\nmAnCuyEB\naREEaxsx\nSZiOvsZx\nEXRBltuL\nEqbhhsxt"

        assertHorMirror "lVHt\nJVhv\nCSbg\nyeCt" "yeCt\nCSbg\nJVhv\nlVHt"
        assertHorMirror "njMK\ndbrZ\nLPKo\ncEYz" "cEYz\nLPKo\ndbrZ\nnjMK"
        assertHorMirror "QMxo\ntmFe\nWLUG\nowoq" "owoq\nWLUG\ntmFe\nQMxo"
        assertHorMirror "FYvi\ndZQn\nuGef\nQoSy" "QoSy\nuGef\ndZQn\nFYvi"