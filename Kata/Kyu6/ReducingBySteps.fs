namespace Kata.Kyu6

module ReducingBySteps =

    open NUnit.Framework
    open FsUnit

    let operArray fct arr init =
        let rec loop prev acc = function
            | [] -> List.rev acc
            | x::xs -> 
                let r = fct prev x 
                loop r (r::acc) xs
        loop init [] arr

    let som = (+)
    let mini = min
    let maxi = max
    let rec gcd x y = if y = 0 then x else gcd y (x % y)
    let gcdi x y = gcd (abs x) (abs y)
    let lcm x y = abs (x * y) / (gcdi x y)
    let lcmu x y = lcm (abs x) (abs y)

    [<Test>]
    let ``Reducing by steps`` () =
        let a = [ 18; 69; -90; -78; 65; 40 ]
        let r1 = [ 18; 3; 3; 3; 1; 1 ]
        operArray gcdi a a.[0] |> should equal r1

        let r2 = [ 18; 414; 2070; 26910; 26910; 107640 ]
        operArray lcmu a a.[0] |> should equal r2

        let r3 = [ 18; 87; -3; -81; -16; 24 ]
        operArray som a 0 |> should equal r3

        let r4 = [ 18; 18; -90; -90; -90; -90 ]
        operArray mini a a.[0] |> should equal r4                

        let r5 = [ 18; 69; 69; 69; 69; 69 ]
        operArray maxi a a.[0] |> should equal r5