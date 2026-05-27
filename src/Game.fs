module AnimalShogi.Game

open Types
open Board
open Rules
open Display

let parseKind (s: string) =
    match s with
    | "Lio"|"lio"   -> Some Lion
    | "Gir"|"gir"   -> Some Giraffe
    | "Ele"|"ele" -> Some Elephant
    | "Chi"|"chi" -> Some Chick
    | "Hen"|"hen"     -> Some Hen
    | _        -> None

let parsePos r c =
    let row = r - 1
    let col = c - 1
    if inBounds row col then Some { Row = row; Col = col }
    else None

type Command =
    | Move of Pos * Pos
    | Drop of PieceKind * Pos
    | Help
    | GiveUp
    | Invalid of string

let parseCommand (line: string) =
    let parts = line.Trim().Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    match parts with
    | [| "help" |] -> Help
    | [| "giveup" |] | [| "give"; "up" |] -> GiveUp
    | [| "move"; r1; c1; r2; c2 |] ->
        match System.Int32.TryParse r1, System.Int32.TryParse c1,
              System.Int32.TryParse r2, System.Int32.TryParse c2 with
        | (true, ir1), (true, ic1), (true, ir2), (true, ic2) ->
            match parsePos ir1 ic1, parsePos ir2 ic2 with
            | Some from, Some dest -> Move (from, dest)
            | _ -> Invalid "row should be 1~4, column should be 1~3"
        | _ -> Invalid "Enter number"
    | [| "drop"; kindStr; r; c |] ->
        match parseKind kindStr with
        | None -> Invalid (kindStr+"is not a valid animal")
        | Some kind ->
            match System.Int32.TryParse r, System.Int32.TryParse c with
            | (true, ir), (true, ic) ->
                match parsePos ir ic with
                | Some dest -> Drop (kind, dest)
                | None -> Invalid "Check range of row and column"
            | _ -> Invalid "Enter number"
    | _ -> Invalid "Not a valid comand"

let validateMove (state: GameState) (from: Pos) (dest: Pos) =
    let b = state.Board
    let player = state.CurrentPlayer
    match getCell b from with
    | None -> Error "There is no animal"
    | Some piece when piece.Owner <> player -> Error "Not your animal"
    | Some piece ->
        let valid = reachableSquares b from piece
        if valid |> List.contains dest then Ok ()
        else Error "Illegal move"

let validateDrop (state: GameState) (kind: PieceKind) (dest: Pos) =
    let player = state.CurrentPlayer
    let captured = if player = P1 then state.CapturedByP1 else state.CapturedByP2
    if not (captured |> List.contains kind) then
        Error ("You do not have that animal")
    else
        let valid = dropPositions state kind
        if valid |> List.contains dest then Ok ()
        else Error "Invalid square"

let rec processTurn (state: GameState) : GameState * GameResult =
    printf "> "
    let line = System.Console.ReadLine()
    if line = null then
        state, Winner (opponent state.CurrentPlayer)
    else
    match parseCommand line with
    | GiveUp ->
        state, Winner (opponent state.CurrentPlayer)
    | Help ->
        printHelp ()
        processTurn state
    | Invalid msg ->
        printfn "❌ %s" msg
        processTurn state
    | Move (from, dest) ->
        match validateMove state from dest with
        | Error msg ->
            printfn "❌ %s" msg
            processTurn state
        | Ok () ->
            let prevPending = state.PendingLionWin
            let newState, result = applyMove state from dest
            match checkPendingLionWin prevPending newState with
            | Winner p -> newState, Winner p
            | Ongoing -> newState, result
    | Drop (kind, dest) ->
        match validateDrop state kind dest with
        | Error msg ->
            printfn "❌ %s" msg
            processTurn state
        | Ok () ->
            let prevPending = state.PendingLionWin
            let newState = applyDrop state kind dest
            match checkPendingLionWin prevPending newState with
            | Winner p -> newState, Winner p
            | Ongoing -> newState, Ongoing

let opponent p = match p with P1 -> P2 | P2 -> P1

let rec gameLoop (state: GameState) =
    printBoard state
    printCaptures state
    printTurn state
    let newState, result = processTurn state
    match result with
    | Winner player ->
        printBoard newState
        printWinner player
    | Ongoing ->
        gameLoop newState
