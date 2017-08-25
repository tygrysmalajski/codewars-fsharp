namespace Kata.Kyu7

// https://www.codewars.com/kata/moves-in-squared-strings-i

open NUnit.Framework
open Kata.MovesInSquaredStrings

module MovesInSquaredStringsI =
    let vertMirror = Seq.map (mapString Array.rev)
    let horMirror = Seq.rev

    [<Test>]
    let ``Moves in squared strings I`` () =
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