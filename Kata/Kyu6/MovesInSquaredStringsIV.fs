namespace Kata.Kyu6

// https://www.codewars.com/kata/moves-in-squared-strings-iv

open NUnit.Framework
open Kata.MovesInSquaredStrings
open Kata.Kyu6.MovesInSquaredStringsIII

module MovesInSquaredStringsIV =
    let diag2Sym = rot90Clock >> Seq.rev
    let rot90Counter = diag1Sym >> Seq.rev
    let selfieDiag2Counterclock = zipConcat [id; diag2Sym; rot90Counter]

    [<Test>]
    let ``Moves in squared strings (IV)`` () =
        let assertDiag2Sym = assertOper diag2Sym
        let assertRot90Clock = assertOper rot90Counter
        let assertSelfieAndDiag1 = assertOper selfieDiag2Counterclock

        assertDiag2Sym "LmvLyg\nDKELBm\nylJhui\nXRXqHD\nzlisCT\nhPqxYb"
            "bTDimg\nYCHuBy\nxsqhLL\nqiXJEv\nPlRlKm\nhzXyDL"
        assertDiag2Sym "sUFjCG\nSHEMPB\nDXJnPY\npORQDN\noVkKuG\nxkxBHU"
            "UGNYBG\nHuDPPC\nBKQnMj\nxkRJEF\nkVOXHU\nxopDSs"
        assertDiag2Sym "dSgpFg\nYyjAlB\nkjknHz\nbBFKCH\nugzIzV\nXqOSMs"
            "sVHzBg\nMzCHlF\nSIKnAp\nOzFkjg\nqgBjyS\nXubkYd"

        assertRot90Clock "EcGcXJ\naaygcA\nNgIshN\nyOrCZE\neBEqpm\nNkxCgw"
            "JANEmw\nXchZpg\ncgsCqC\nGyIrEx\ncagOBk\nEaNyeN"
        assertRot90Clock "iMUS\nsqdd\nAoHs\nCJTa" "Sdsa\nUdHT\nMqoJ\nisAC"
        assertRot90Clock "FtKrTJ\nfQOJvH\nYWWnyE\nmmSrKr\nuNMxrp\nIaPvfq"
            "JHErpq\nTvyKrf\nrJnrxv\nKOWSMP\ntQWmNa\nFfYmuI"

        assertSelfieAndDiag1 "NJVGhr\nMObsvw\ntPhCtl\nsoEnhi\nrtQRLK\nzjliWg"
            "NJVGhr|gKilwr|rwliKg\nMObsvw|WLhtvh|hvthLW\ntPhCtl|iRnCsG|GsCnRi\nsoEnhi|lQEhbV|VbhEQl\nrtQRLK|jtoPOJ|JOPotj\nzjliWg|zrstMN|NMtsrz"
        assertSelfieAndDiag1 "JAAn\nsrpa\nFngg\nmrVJ"
            "JAAn|Jgan|nagJ\nsrpa|VgpA|ApgV\nFngg|rnrA|Arnr\nmrVJ|mFsJ|JsFm"
        assertSelfieAndDiag1 "MpbD\nfxUT\ndUfr\nbtbs"
            "MpbD|srTD|DTrs\nfxUT|bfUb|bUfb\ndUfr|tUxp|pxUt\nbtbs|bdfM|Mfdb"  
        