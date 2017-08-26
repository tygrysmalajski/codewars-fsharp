namespace Kata.Kyu6

// https://www.codewars.com/kata/moves-in-squared-strings-ii

open NUnit.Framework
open Kata.Kyu7.MovesInSquaredStringsI

module MovesInSquaredStringsII =
    let private append f1 f2 (ss: seq<string>) =  Seq.append (f1 ss) (f2 ss)
    let private dot = 
        let appendDots (s: string) = s + ("." |> String.replicate s.Length)
        Seq.map appendDots

    let rot = vertMirror >> horMirror
    let selfieAndRot = append dot (dot >> rot)

    [<Test>]
    let ``Moves in squared strings (II)`` () =
        let assertRot = assertOper rot
        let assertSelfieAndRot = assertOper selfieAndRot

        assertRot "rkKv\ncofM\nzXkh\nflCB" "BClf\nhkXz\nMfoc\nvKkr"
        assertRot "fijuoo\nCqYVct\nDrPmMJ\nerfpBA\nkWjFUG\nCVUfyL" "LyfUVC\nGUFjWk\nABpfre\nJMmPrD\ntcVYqC\nooujif"
        assertRot "lVHt\nJVhv\nCSbg\nyeCt" "tCey\ngbSC\nvhVJ\ntHVl"
        assertRot "QMxo\ntmFe\nWLUG\nowoq" "qowo\nGULW\neFmt\noxMQ"

        assertSelfieAndRot "xZBV\njsbS\nJcpN\nfVnP"
            "xZBV....\njsbS....\nJcpN....\nfVnP....\n....PnVf\n....NpcJ\n....Sbsj\n....VBZx"
        assertSelfieAndRot "uLcq\nJkuL\nYirX\nnwMB"
            "uLcq....\nJkuL....\nYirX....\nnwMB....\n....BMwn\n....XriY\n....LukJ\n....qcLu"
        assertSelfieAndRot  "lVHt\nJVhv\nCSbg\nyeCt"
            "lVHt....\nJVhv....\nCSbg....\nyeCt....\n....tCey\n....gbSC\n....vhVJ\n....tHVl"
        assertSelfieAndRot  "QMxo\ntmFe\nWLUG\nowoq"
            "QMxo....\ntmFe....\nWLUG....\nowoq....\n....qowo\n....GULW\n....eFmt\n....oxMQ"