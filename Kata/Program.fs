open System.Collections.Generic

[<EntryPoint>]
let main argv = 
    let isValidWalk =
        List.fold (fun (i, n, s, e, w) c -> 
            match c with
            | _ when c = 'n' -> (i+1, n+1, s, e, w)
            | _ when c = 's' -> (i+1, n, s+1, e, w)
            | _ when c = 'e' -> (i+1, n, s, e+1, w)
            | _ when c = 'w' -> (i+1, n, s, e, w+1)
            | _ -> failwith "Invalid direction"
        ) (0, 0, 0, 0, 0)
        >> (fun (i, n, s, e, w) -> i = 10 && n = s && e = w)

    isValidWalk ['n';'s';'n';'s';'n';'s';'n';'s';'n';'s']
    isValidWalk ['n';'s';'n';'s';'n';'s';'n';'s';'n';'n']
    0
