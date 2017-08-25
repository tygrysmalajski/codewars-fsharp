namespace Kata.Kyu6

// https://www.codewars.com/kata/55e2adece53b4cdcb900006c

module TortoiseRacing =

    open NUnit.Framework
    open FsUnit

    let race v1 v2 g =
        if v1 < v2 then
            let delta1 = float(v1) / float(3600)
            let delta2 = float(v2) / float(3600)
            Seq.unfold 
                (fun (distance1, distance2) -> 
                    let time = distance1 / float(v2)
                    if distance2 < distance1 then 
                        Some(time, (distance1+delta1, distance2+delta2)) 
                    else 
                        None
                ) (float(g), 0.0)
            |> Seq.last
            |> (fun t -> 
                let time = System.TimeSpan.FromHours(t)
                Some([time.Hours; time.Minutes; time.Seconds]))
        else 
            None

    [<Test>]
    let ``Tortoise racing`` () =
        let assertRace v1 v2 g result =
            race v1 v2 g |> should equal result

        assertRace 720 850 70 (Some [0; 32; 18])
        assertRace 80 100 40 (Some [2; 0; 0])
        assertRace 80 91 37 (Some [3; 21; 49])
        assertRace 80 80 3700 None