open System

type Move =
    | Rock
    | Paper
    | Scissors

type Result =
    | Win
    | Loss
    | Draw    

// Determines result for player1
let game player1 player2 = 
    match player1,player2 with
    | Rock, Scissors -> Win
    | Rock, Paper -> Loss
    | Paper, Rock -> Win
    | Paper, Scissors -> Loss
    | Scissors, Paper -> Win
    | Scissors, Rock -> Loss
    | _,_ -> Draw

// Generates a random move
let randomMove() =
    let n = new Random () |> fun r -> r.Next(3)
    match n with
        | 0 -> Rock
        | 1 -> Paper
        | 2 -> Scissors

let stringToMove(s) = 
    match s with
    | "rock" -> Rock
    | "paper" -> Paper
    | "scissors" -> Scissors
    | _ -> exit 1

let printResult(r) =
    match r with
    | Win -> printfn "You won!"
    | Loss -> printfn "You lose. Better luck next time!"
    | Draw -> printfn "It's a draw"
    
[<EntryPoint>]
let main argv =
    let sc = Kattio.Scanner()
    
    printfn "Let's play Rock, Paper, Scissors!"
    printfn "Enter your move (rock/paper/scissors): "
    let playerMove = sc.Next() |> stringToMove
    let rndMove = randomMove()
    let result = game playerMove rndMove

    printfn "Opponent played %A"  rndMove
    printResult(result)
    0