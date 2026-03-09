module Exam2022

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale


    let img =
        Quad (
        Square 255uy,
        Square 128uy,
        Quad(Square 255uy,
            Square 128uy,
            Square 192uy,
            Square 64uy),
            Square 0uy)

    let countWhite img  = 
        let rec helper img acc = 
            match img with 
            | Square s when s = 255uy -> acc + 1
            | Square s -> acc
            | Quad (g1, g2, g3, g4) -> helper g1 acc |> helper g2 |> helper g3 |> helper g4
        helper img 0

    let rec rotateRight img = 
            match img with 
                | Square s -> img
                | Quad (g1, g2, g3, g4) -> Quad (rotateRight g4, rotateRight g1, rotateRight g2, rotateRight g3)

    let rec map (mapper : uint8 -> grayscale) img =
        match img with
        | Square s -> mapper s
        | Quad (g1, g2, g3, g4) -> Quad(map mapper g1, map mapper g2, map mapper g3, map mapper g4)

    let bitmap = map (fun x -> if x <= 127uy then Square (0uy) else Square (255uy))

    let rec fold (folder : 'a -> uint8 -> 'a) (acc : 'a) img = 
        match img with 
        | Square s -> folder acc s
        | Quad (g1, g2, g3, g4) -> (fold folder acc g1)
                                                                                |> fold folder <| g2
                                                                                |> fold folder <| g3
                                                                                |> fold folder <| g4

    let countWhite2 img = fold (fun acc x -> if int x = 255 then acc + 1 else acc + 0) 0 img

    // 51 min

    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | [] -> []
        | x :: xs -> (foo x) :: (bar xs)

    (*
    // 2.1 
    // types: 
    // foo:
    // int -> string
    // bar:
    // list<int> -> list<string>

    // What do they do: 
    // foo takes in an int x and will build a binary number. 
    // If x is even it will append "0" and if its uneven it will append "1" 
    // it will return the string when x = 0. Every time it appends a number it will divide x with 2

    // bar takes a list of ints and runs foo on every element. 
    // Every binary number foo generates gets prepended and eventually 
    // returned when there are no more numbers in the list

    // Names:
    // foo = binaryGeneratorSingle
    // bar = binaryGeneratorMultiple

    // Something wrong with foo
    // foo will not work with negative numbers so x must be non negative 

    // 2.2
    // Warning:
    // It compiles with the warning "incomplete pattern matches". This indicates that there are match cases for x that have not been covered 
    // this could result in exceptions being raised if not careful *)

    let rec foo2 =
        function
        | 0 -> ""
        | x when x < 0  -> failwith 
                                "negative number not allowed"
        | x when x % 2 = 0 && x > 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 && x > 0 -> foo (x / 2) + "1"
        | x -> ""

        // 2.3

    let bar2 = 
        function 
        | [] -> []
        | lst -> List.fold (fun acc elem -> (foo elem)::acc) [] lst

    (* // 2.4 

    // lets look at foo 
    // (foo 10) -> ((foo 10) + foo 5) -> (((foo 10) + foo 5) + foo 2) -> ((((foo 10) + foo 5) + foo 2) + foo 1) 
    // -> (((((foo 10) + foo 5) + foo 2) + foo 1) + foo 0) -> (((((foo 10) + foo 5) + foo 2) + foo 1) + "") 
    // -> (((((foo 10) + foo 5) + foo 2) + "1") + "") -> (((((foo 10) + foo 5) + "0") + "1") + "")
    // -> (((((foo 10) + "1") + "0") + "1") + "") -> ((((("0") + "1") + "0") + "1") + "") -> "0101"

    // the problem with this function is that we have to start a recursive call all the way till x = 0 before we can start evaluating
    // This means we have to add a frame to the stack 5 times here before we can start going back up. The problem is very clear at this point
    // (((((foo 10) + foo 5) + foo 2) + foo 1) + foo 0) we need to store all these calls on the stack which is expensive

    // I would say that foo is the function that is most likely to overflow with a 
    //big enough number you could end up with enough recursive calls
    // to overflow (stack limit is only around 10000) *)

    // 2.5 

    let fooTail x = 
        let rec looper x acc  = 
            match x with
            | 0 -> acc
            | x when x % 2 = 0 && x > 0 -> acc+"0" |> looper (x/2)
            | x when x % 2 = 1 && x > 0 -> acc+"1" |> looper (x/2)

        looper x ""

    // 2.6

    let barTail (lst : list<int>) = 
        let rec looper (lst : list<int>) (temp : list<string>)  c = 
            match lst with
            | [] -> c temp 
            | x::xs -> looper xs temp (fun r -> c ((foo x)::r))

        looper lst [] id


    // 3.1

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
        printfn ""

    // Invalid matrix dimensions: m1 rows = <number of rows in m1>, m1 columns = <number of
    // columns in m1>, m2 roms = <number of rows in m2>, m2 columns = <number of columns in m2>

    let failDimensions m1 m2 = failwith (sprintf "Invalid matrix dimensions: m1 rows 
                    = %i, m1 columns = %i, m2 roms = %i, m2 columns = %i" (numRows m1) (numCols m1) (numRows m2) (numCols m2))

    // 107 min

    // 3.2

    let add m1 m2 = 
        match m1, m2 with
        | m1, m2 when numRows m1 = numRows m2 && numCols m1 = numCols m2 -> 
            init (fun x y -> ((get m1 x y) + (get m2 x y))) (numRows m1) (numCols m1) 
        | m1, m2 -> failDimensions m1 m2


    // 3.3
    // 127.5 min

    //let dotProduct m1 m2 r c = 


    // 4 Stack Machines

    // 4.1

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

    type stack = list<int>

    let emptyStack () : stack = stack.Empty

    let runStackProg (p : stackProgram) = 
        let rec aux (p : stackProgram) (stack : stack) =  
            match p with
            | [] -> stack.Head
            | c::ps -> match c with 
                        | Push x -> aux ps ([x] @ stack)
                        | Add when List.length stack > 1 -> aux ps ( [stack.Head + stack.Tail.Head]  @ stack.Tail.Tail)
                        | Mult when List.length stack > 1 -> aux ps ( [stack.Head * stack.Tail.Head] @ stack.Tail.Tail)
                        | _ -> failwith "empty stack"
        match p with
        | [] -> failwith "empty stack"
        | p -> aux p (emptyStack ()) 


    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail = SM (fun _ -> None)

    let bind f (SM a) : StateMonad<'b> =
        SM (fun s ->
        match a s with
        | Some (x, s') ->
        let (SM g) = f x
        g s'
        | None -> None)

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    // 4.3

    let push x = SM (fun stack  -> (Some((), [x]@stack)))

    let pop = SM (fun stack -> if List.length stack > 0 then (Some(stack.Head,stack.Tail)) else None) // this is missing fail

    type StateBuilder() =
        member this.Bind(f, x) = bind x f
        member this.Return(x) = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)
    let state = new StateBuilder()

    let runStackProg2 prog = 
        let rec aux prog = 
            match prog with
            | [] -> pop
            | c::ps -> match c with
                        | Push x->  push x >>= (fun _ -> aux ps)
                        | Add -> pop >>= (fun x -> pop >>= (fun y ->  push (x + y))) >>= (fun _ -> aux ps)
                        | Mult -> pop >>= (fun x -> pop >>= (fun y -> push (x * y))) >>= (fun _ -> aux ps)
        aux prog

    // 127+79 min    
    // 4.5 

    open JParsec.TextParser

    let whitespaceChar =
        satisfy System.Char.IsWhiteSpace <?> "whitespace"

    let spaces = many whitespaceChar <?> "space"

    let (.>*>) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces .>> p2
    let (>*>.) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces >>. p2

    let pspush = pstring "PUSH"
    let psadd = pstring "ADD"
    let psmult = pstring "MULT"

    let newline = pchar '\n'

    let lineparser parser mapper label =
        spaces
        >>. parser
        |>> mapper <?> label

    let ppush = lineparser (pspush >*>. pint32) (Push) "push"
    let padd = lineparser (psadd) (fun _ -> Add) "add"
    let pmul = lineparser (psmult) (fun _ -> Mult) "mult"

    let pprog = 
        (choice [ppush; padd; pmul])
        |> many1

    (* 
        ================
        Please read me
        ================
        The assignment says that parseStackProg must be of type "string -> ParserResult<stackProgram>"
        The only way of getting a ParserResult from JParsec is to run the parser, you can then get the result afterwards
        My problem here is that according to the example :
            
            "PUSH 5\nPUSH 4     \nADD\n    PUSH8\nMULT          \n"
            |> run parseStackProg
            |> getSuccess
            ... (more code)
        
        we are supposed to output a Parser<stackProgram> which is inputted into run, that directly conflicts with the information written in the assignment
        The "correct" example would be
        
            "PUSH 5\nPUSH 4     \nADD\n    PUSH8\nMULT          \n"
            parseStaackProg
            |> getSuccess
            ... (more code)
        
        That does not suit the way we usually do these assignments though.
        I'm going to follow the information of the assignment and not the example, i've commented a solution below it that fits the example, i hope thats okay
        PS. i tried to highlight this to the exam inviligators(?), but failed to explain it properly 
    *)
    let parseStackProgram str =
        run pprog str

    // let parseStackProg =
    //     pprog


    let parseStackProg = parseStackProgram




