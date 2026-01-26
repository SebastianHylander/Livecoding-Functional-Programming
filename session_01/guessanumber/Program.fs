
open System

let sc = Kattio.Scanner()
let n = new Random () |> fun r -> 1 + r.Next (100) 

let rec guess () = 
    let i = sc.NextInt()
    if i = n then
        printfn "Correct the number was %d" n
        1
    else 
        if i < n then
            printfn "Your guess is too low"
        else 
            printfn "Your guess is too high"
        
        guess() + 1 
    

printfn "Welcome to the Guessing game!"
printfn "I have a number between 1 and 100"
printfn "Get guessing!"
printfn "You used %d guesses" (guess ())