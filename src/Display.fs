module AnimalShogi.Display

open Types
open Board

let pieceChar kind player =
    let s =
        match kind with
        | Lion     -> "Lio"
        | Giraffe  -> "Gir"
        | Elephant -> "Ele"
        | Chick    -> "Chi"
        | Hen      -> "Hen"
    match player with
    | P1 -> "1" + s
    | P2 -> "2" + s

let cellStr cell =
    match cell with
    | None -> "    "
    | Some p -> pieceChar p.Kind p.Owner

let printBoard (state: GameState) =
    let b = state.Board
    printfn ""
    printfn "     Col1   Col2   Col3"
    printfn "   +------+------+------+"
    for r in 0..3 do
        let area =
            if r = 0 then " [P2 Area]"
            elif r = 3 then " [P1 Area]"
            else ""
        printf "Row%d |" (r + 1)
        for c in 0..2 do
            printf " %s |" (cellStr b.[r, c])
        printfn "%s" area
        printfn "   +------+------+------+"
    printfn ""

let printCaptures (state: GameState) =
    let print lst =
        if lst = [] then "(None)"
        else
            lst
            |> List.map (fun k ->
                match k with
                | Lion -> "Lio" | Giraffe -> "Gir" | Elephant -> "Ele"
                | Chick -> "Chi" | Hen -> "Hen")
            |> String.concat ", "
    printfn "  P1 Captured: %s" (print state.CapturedByP1)
    printfn "  P2 Captured: %s" (print state.CapturedByP2)
    printfn ""

let printTurn (state: GameState) =
    let p = match state.CurrentPlayer with P1 -> "Player 1" | P2 -> "Player 2"
    printfn "=== %s's turn ===" p
    match state.PendingLionWin with
    | Some (lion_player, _) when lion_player <> state.CurrentPlayer ->
        printfn "⚠️  Warning: opponent's lion is in your area"
    | _ -> ()

let printHelp () =
    printfn "Comand:"
    printfn "  move <row> <col> <row> <col>  ex) move 3 2 2 2  (3r2c → 2r2c)"
    printfn "  drop <Animal> <row> <col>     ex) drop Chi 3 2  (Drop Chi on 3r1c)"
    printfn "  help "
    printfn "  give up"
    printfn ""
    printfn "Animals : Lio, Gir, Ele, Chi, Hen"
    printfn ""

let printWinner (player: Player) =
    let name = match player with P1 -> "Player 1" | P2 -> "Player 2"
    printfn ""
    printfn "╔══════════════════════════╗"
    printfn "║   🎉 %s win! 🎉    ║" name
    printfn "╚══════════════════════════╝"