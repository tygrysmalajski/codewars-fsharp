namespace Kata.Kyu5

// https://www.codewars.com/kata/closest-and-smallest

open FsUnit
open NUnit.Framework

module ClosestAndSmallest =
    let closest (string: string) =
        let split (s: string) = s.Split([|' '|])
        let parse = System.Int32.Parse
        let weigh = 
            Seq.map (fun c -> c.ToString()) 
            >> Seq.map parse 
            >> Seq.sum
        let closest' s =
            let weights = 
                split s
                |> Seq.mapi (fun i n -> ((weigh n), i, (parse n)))
                |> Seq.sortBy (fun (x, _, _) -> x)
            weights
            |> Seq.allPairs weights
            |> Seq.filter (fun (n1, n2) -> n1 <> n2)
            |> Seq.minBy (fun ((w1, i1, _), (w2, i2, _)) -> (abs(w1-w2), w1, w2, i1, i2))
            |> (fun ((w1, i1, n1), (w2, i2, n2)) -> ([w1;i1;n1], [w2;i2;n2]))

        if string = "" then 
            ([],[])
        else
            closest' string

    [<Test>]
    let ``Closest and Smallest`` () =
        let assertClosest s (expected: list<int> * list<int>) =
            closest s |> should equal expected

        assertClosest "" ([],[])
        assertClosest "103 123 4444 99 2000" ([2;4;2000],[4;0;103])
        assertClosest "456899 50 11992 176 272293 163 389128 96 290193 85 52" ([13; 9; 85], [14; 3; 176])
        assertClosest "239382 162 254765 182 485944 134 468751 62 49780 108 54" ([8; 5; 134], [8; 7; 62])
        assertClosest "241259 154 155206 194 180502 147 300751 200 406683 37 57" ([10; 1; 154], [10; 9; 37])
        assertClosest "89998 187 126159 175 338292 89 39962 145 394230 167 1" ([13; 3; 175], [14; 9; 167])
        assertClosest "462835 148 467467 128 183193 139 220167 116 263183 41 52" ([13; 1; 148], [13; 5; 139])
        assertClosest "403749 18 278325 97 304194 119 58359 165 144403 128 38" ([11; 5; 119], [11; 9; 128])
        assertClosest "28706 196 419018 130 49183 124 421208 174 404307 60 24" ([6; 9; 60], [6; 10; 24])
        assertClosest "189437 110 263080 175 55764 13 257647 53 486111 27 66" ([8; 7; 53], [9; 9; 27])
        assertClosest "79257 160 44641 146 386224 147 313622 117 259947 155 58" ([11; 3; 146], [11; 9; 155])
        assertClosest "315411 165 53195 87 318638 107 416122 121 375312 193 59" ([15; 0; 315411], [15; 3; 87])
        assertClosest "39405 196 14425 148 66954 149 309426 34 459176 24 12" ([16; 1; 196], [16; 2; 14425])
        assertClosest "233933 53 162068 127 259380 66 253442 79 390644 117 1" ([23; 0; 233933], [23; 2; 162068])
        assertClosest "18640 61 432000 161 269331 102 139284 151 302233 87 69" ([7; 1; 61], [7; 7; 151])
        assertClosest "440162 48 249545 166 481368 27 375775 3 466816 192 32" ([12; 1; 48], [12; 9; 192])
        assertClosest "435577 180 152979 34 493394 188 255188 177 211346 197 78" ([15; 7; 177], [15; 10; 78])
        assertClosest "110995 19 77691 191 115400 184 14172 52 17520 161 98" ([11; 3; 191], [11; 4; 115400])
        assertClosest "409594 13 197979 55 280823 133 88517 151 262385 138 47" ([7; 5; 133], [7; 7; 151])
        assertClosest "214751 46 434261 111 240291 79 149210 54 64735 44 27" ([9; 7; 54], [9; 10; 27])
        assertClosest "169572 145 129032 41 114712 19 328036 108 317281 86 91" ([10; 1; 145], [10; 5; 19])
        assertClosest "386675 135 478288 71 58392 143 294086 170 432051 103 18" ([8; 3; 71], [8; 5; 143])
