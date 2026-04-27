module Exam2024

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find when switching back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2024 = 
 *)

(* 1: Transactions *)

    type transactions =
        | Empty
        | Pay     of string * int * transactions
        | Receive of string * int * transactions
        
    let rec balance trs = match trs with
                            | Empty -> 0
                            | Pay (_,amount,next_trs)  -> balance next_trs - amount
                            | Receive (_,amount,next_trs)  -> balance next_trs + amount
        
    let rec balAcc trs acc = match trs with
                               | Empty -> acc
                               | Pay (_,amount,next_trs) -> acc - amount |> balAcc next_trs
                               | Receive (_,amount,next_trs) -> acc + amount |> balAcc next_trs

    let balanceAcc trs = balAcc trs 0
        
    let rec participants trs = match trs with
                                | Empty -> (Set.empty,Set.empty)
                                | Pay (name, _, next_trs) -> let part = participants next_trs
                                                             (fst part |> Set.add name, snd part)
                                | Receive (name, _, next_trs) -> let part = participants next_trs
                                                                 (fst part, snd part |> Set.add name)

    
    
    let rec balanceFold payFolder receiveFolder acc trs = match trs with
                                                          | Empty -> acc
                                                          | Pay (name, amount, next_trs) -> (payFolder acc name amount |> balanceFold payFolder receiveFolder) next_trs
                                                          | Receive (name, amount, next_trs) -> (receiveFolder acc name amount |> balanceFold payFolder receiveFolder) next_trs
    
    let collectFolder op map name amount = match Map.containsKey name map with
                                            | true -> Map.add name (op (Map.find name map) amount) map
                                            | false -> Map.add name (op 0 amount) map

    let collect trs = balanceFold (collectFolder (-)) (collectFolder (+)) Map.empty trs
    
    
(* 2: Code Comprehension *)
        
    let foo (x : char) = x |> int |> fun y -> y - (int '0')
    
    let bar (x : string) = [for c in x -> c]
            
    let rec baz =
        function
        | [] -> 0
        | x :: xs -> x + 10 * baz xs
    
(* Question 2.1 *)

    (*     
    Q: What are the types of functions foo, bar, and baz?

    A: foo: char -> int
       bar: string -> char list
       baz: int list -> int


    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: foo: takes a characters integer-value and returns that integer
       bar: takes a string and returns a list of all the characters in the string in the same order
       baz: takes a list of integers and returns the sum of all these where the result is multiplied by 10 for each step. This makes the list where all integers are smaller than 10 result in the the integesers getting concatinated backward, like the following: baz [2;5;3;8] -> 8352
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: charToInt
       bar: stringToCharList
       baz: foldIntListToInt
    
    Q: The function foo only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: One could argue that the intended behavior of foo is to only return the values of the specific integer-character such as '1', '3' or '7' where foo in this scenario would also return the values of other characters such as 'a', 'm' or '/'. 
       This means the contraint should be that x has to be in '0' .. '9'
    
    Q: The function baz only behaves reasonably if certain 
       constraint(s) are met on its argument. 
       What is/are these constraints?
        
    A: Assuming the intended output is the 'backwards concatinated' integer of the input all the integers in the inputtet list has to be in the following interval 0 .. 9
    *)

(* Question 2.2 *)
    
    let stringToInt = bar >> List.rev >> List.map foo >> baz

(* Question 2.3 *)
    
    let baz2 intlst = List.foldBack (fun n acc -> 10*acc + n) intlst 0
    
(* Question 2.4 *)

    (*

    Q: The function `baz` from Question 2.1 is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).

    A: The function baz is not tail recursive since the operation '10 * baz xs' cannot be evaluated until the recursive call has been made, 
       since it neds the value of 'baz xs' first, meaning that the recursive call is not the last thing to happen in each call.

       e.g.
       baz [2,7,1,4] ->
       2 + 10 * baz [7,1,4] ->
       2 + 10 * (7 + 10 * baz [1,4]) ->
       2 + 10 * (7 + 10 * (1 + 10 * baz [4])) ->
       2 + 10 * (7 + 10 * (1 + 10 * (4 + 10 * baz []))) ->
       2 + 10 * (7 + 10 * (1 + 10 * (4 + 10 * 0))) ->
       2 + 10 * (7 + 10 * (1 + 10 * 4)) ->
       2 + 10 * (7 + 10 * 41) ->
       2 + 10 * 417 ->
       4172

       Here you can see that the result could not be evaluated before the end of the list was reached. 
    *)
    
(* Question 2.5 *)

    let rec bazTailCon intlst con = match intlst with
                                    | [] -> con 0
                                    | x :: xs -> bazTailCon xs (fun i -> x + 10 * con i)

    let bazTail intlst = bazTailCon (List.rev intlst) id

        
(* 3: Caesar Ciphers *)

(* Question 3.1 *)
    
    let rec check_int i = if i > int 'z' then i - 26 |> check_int else if i < int 'a' then i + 26 |> check_int else i

    let encryptchar offset c = match c with
                               | ' ' -> " "
                               | c ->  int c + offset |> check_int |> char |> string


    let encrypt code offset = String.collect (encryptchar offset) code
    
(* Question 3.2 *)
    let decrypt code offset = encrypt code (offset * -1)
    
(* Question 3.3 *)
    let rec decode_rec plainText encoded key = match key with
                                               | 26 -> None
                                               | n -> if encrypt plainText key = encoded then Some key else decode_rec plainText encoded (key+1)

    let decode plainText encoded = decode_rec plainText encoded 0
    
(* Question 3.4 *)
    
    let rec buildString lst = match lst with
                              | [] -> ""
                              | x :: xs -> buildString xs + " " + x 

    let parEncrypt (code : string) offset = code.Split ' ' |> (Array.fold (fun acc str -> async { return encrypt str offset } :: acc ) []) |> Async.Parallel |> Async.RunSynchronously |> Array.toList |> buildString
    
(* Question 3.5 *)
        
    open JParsec.TextParser

    let pcharacter = satisfy (fun c -> if int c > int 'z' || int c < int 'a' then false else true)

    let pspace = satisfy (fun c -> if int c = int ' ' then true else false)

    let pcharorspace = choice [pcharacter;pspace]



    let parseEncrypt (offset : int) = many pcharorspace |>> (fun lst -> List.foldBack (fun c acc -> (encrypt (string c) offset) + acc) lst "")

(* 4: Letterboxes *)
    
(* Question 4.1 *)
    
    type letterbox = Inbox of Map<string, string list>
    
    let empty () = Inbox (Map.empty)

(* Question 4.2 *)

    let rec post_last message inbox = match inbox with
                                      | [] -> [message]
                                      | x :: xs -> x :: post_last message xs
    
    let post sender message (Inbox mb) = match Map.containsKey sender mb with
                                         | true -> Inbox ((Map.find sender mb |> post_last message |> Map.add sender) mb)
                                         | false -> Inbox (Map.add sender [message;] mb)
    
    let read sender (Inbox mb) = match Map.containsKey sender mb with
                                 | true -> Some (Map.find sender mb |> List.head, Inbox(Map.add sender (Map.find sender mb |> List.tail) mb))
                                 | false -> None

    
(* Question 4.3 *)
    type StateMonad<'a> = SM of (letterbox -> ('a * letterbox) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM (SM f) = f (empty ())
    

    let post2 sender message = SM (fun lb -> Some ((),post sender message lb))

    let read2 sender = SM (fun lb -> match read sender lb with
                                     | Some (str, lb) -> Some (str, lb)
                                     | None -> None
                          )

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    type MType =
        | Post of string * string
        | Read of string
    type log = MType list
    
    let rec trace log = match log with (* Forkert *)
                        | [] -> ret []
                        | x :: xs -> match x with
                                     | Post (sender,message) -> post2 sender message >>>= trace xs
                                     | Read sender -> trace xs >>= (fun result -> (read2 sender >>= fun message -> ret(message :: result)))
