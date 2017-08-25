namespace Kata

open FsUnit

module MovesInSquaredStrings =
    let mapString (mapping: char[] -> char[]) (s: string) = new string((s.ToCharArray()) |> mapping)

    let mapString2 (mapping: char[] -> char) = mapString (mapping >> (fun x -> [|x|]))

    let apply map =
        let split (s: string) = s.Split([||])
        let concat = String.concat "\n"
        split 
        >> map
        >> concat

    let oper fct s = fct s

    let assertOper oper s expected =
        s |> apply oper |> should equal expected