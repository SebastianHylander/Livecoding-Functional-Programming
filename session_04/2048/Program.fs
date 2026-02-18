open System

let cellToString cell =
    match cell with
    | None   -> "    "
    | Some n -> sprintf "%4d" n

let printRow row =
    row
    |> List.map cellToString
    |> String.concat " | "
    |> printfn "| %s |"

let printBoard board =
    let size = List.length board
    let horizontal =
        String.replicate (size * 7 + 1) "-"

    printfn "%s" horizontal
    board |> List.iter (fun row ->
        printRow row
        printfn "%s" horizontal)

let getNones m =
    let rec inner lst x y =
        match lst with
        | [] -> []
        | None :: xs -> (x,y) :: inner xs (x+1) y
        | _ :: xs -> inner xs (x+1) y
    
    List.fold (fun acc item -> inner item 0 (snd acc) :: fst acc, snd acc + 1) ([], 0) m
    |> fst
    |> List.collect id


let rec _insertTile x tile lst =
    match x with
    | 0 -> tile :: List.tail lst
    | _ -> List.head lst :: _insertTile (x-1) tile (List.tail lst)

let rec insertTile (x,y) tile m = 
    match y with
    | 0 ->  _insertTile x tile (List.head m) :: List.tail m
    | _ -> List.head m :: insertTile (x,y-1) tile (List.tail m)

let insertRandomTile m =
    let newTile = 
        getNones m
        |> fun nones ->
            System.Random ()
            |> fun rng -> ((rng.Next(2) + 1) * 2 ,List.item (rng.Next nones.Length) nones)

    insertTile (snd newTile) (Some (fst newTile)) m

type Direction =
    | Right
    | Left
    | Up
    | Down

(*let rec read_direction (sc : Kattio.Scanner) =
    match (sc.Next ()).ToLower() with
    | "r" | "right" -> Right
    | "l" | "left"  -> Left
    | "u" | "up"    -> Up
    | "d" | "down"  -> Down
    | _ -> read_direction sc
*)

let rec read_direction () =
    let key = Console.ReadKey(true).Key
    match key with
    | ConsoleKey.LeftArrow  -> Left
    | ConsoleKey.RightArrow -> Right
    | ConsoleKey.UpArrow    -> Up
    | ConsoleKey.DownArrow  -> Down
    | _ -> read_direction ()

let formatMatrix (dir : Direction) = 
    match dir with
    | Right -> id
    | Left -> List.map List.rev 
    | Up -> List.transpose >> List.map List.rev
    | Down -> List.transpose

let formatMatrixBack (dir : Direction) = 
    match dir with
    | Up -> List.map List.rev >> List.transpose
    | _ -> formatMatrix dir

let shift (lst : (int option) list) = 
    List.foldBack (
        fun item acc -> 
            match item with
            | Some(i) -> Some(i) :: fst acc , snd acc
            | None -> fst acc, snd acc + 1
    ) lst ([], 0) 
    |> fun (lst,n) -> List.init n (fun _ -> None) @ lst

let merge (lst : (int option) list) = 
    List.foldBack (
        fun item acc -> 
            match item, snd acc with
            | Some(i), Some(j) when i = j -> Some(i+j) :: fst acc , None
            | _  -> snd acc :: fst acc, item
    ) (None::lst) ([], None) 
    |> fst

let updateRow = shift >> merge >> shift >> List.tail

let move dir m = formatMatrix dir m |> List.map updateRow |> formatMatrixBack dir

let (<=>) m dir = move dir m

let m : (int option) list list = List.init 4 (fun _ -> List.init 4 (fun _ -> None)) |> insertRandomTile |> insertRandomTile



let sc = Kattio.Scanner ()

let rec loop m = 
    System.Console.Clear ()
    printBoard m
    //let dir = read_direction sc
    let dir = read_direction ()
    let next = m <=> dir |> insertRandomTile

    loop next

loop m
