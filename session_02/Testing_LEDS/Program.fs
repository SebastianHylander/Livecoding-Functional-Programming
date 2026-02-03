// For more information see https://aka.ms/fsharp-console-apps
[<EntryPoint>]
let main argv =
    let sc = Kattio.Scanner()

    let count = sc.NextInt()

    let rec helper counter shortest =
        match counter with
        | 0 -> shortest
        | v ->
            (fun (time, status) ->
                match status with
                | 0 when time < shortest -> helper (v - 1) time
                | _ -> helper (v - 1) shortest

            ) (
                sc.Next() |> uint32,
                sc.NextInt()
            )

    let result = helper (count) System.UInt32.MaxValue

    match result with
    | r when r = System.UInt32.MaxValue -> printf "-1"
    | r -> printf "%d" r

    0
