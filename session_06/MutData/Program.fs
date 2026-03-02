let mutable a = 15

printfn "%d" a

a <- 20

printfn "%d" a

// Example from slides

let x = 15

let f y =
    let x = y + 1
    x * x

let result = f 2

printfn "%d" result

let mutable x_mut = 15

let f_mut y =
    x_mut <- y + 1
    x_mut * x_mut

let result_mut = f_mut 6

printfn "%d" result_mut

// Example of issues that can happen with mutations
open System.Collections.Generic

let mutable l = new List<int>([ 1; 2; 4; 8 ])


// Define function that takes unit to delay evaluation
let first () = l.[0]

let malicious_func x =
    l.Clear() // mutation happens here = side effect
    x + 2

let res = malicious_func 2

printfn "%d" (first ())

// We can avoid this by working functionally = no side effects
