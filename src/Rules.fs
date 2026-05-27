module AnimalShogi.Rules

open Types
open Board

let movement owner kind =
    let fwd = if owner = P1 then -1 else 1
    match kind with
    | Lion     -> [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]
    | Giraffe  -> [(-1,0);(0,-1);(0,1);(1,0)]
    | Elephant -> [(-1,-1);(-1,1);(1,-1);(1,1)]
    | Chick    -> [(fwd, 0)]
    | Hen      -> [(fwd,-1);(fwd,1);(-1,0);(0,-1);(0,1);(1,0)]

let reachableSquares (b: Board) (pos: Pos) (piece: Piece) : Pos list =
    movement piece.Owner piece.Kind
    |> List.map (fun (dr, dc) -> { Row = pos.Row + dr; Col = pos.Col + dc })
    |> List.filter (fun p -> inBounds p.Row p.Col)
    |> List.filter (fun p ->
        match getCell b p with
        | Some pc -> pc.Owner <> piece.Owner  
        | None    -> true)

let dropPositions (state: GameState) (kind: PieceKind) : Pos list =
    let b = state.Board
    let player = state.CurrentPlayer
    [ for r in 0..3 do
        for c in 0..2 do
            if b.[r, c] = None then
                let blocked =
                    kind = Chick &&
                    ((player = P1 && r = 0) || (player = P2 && r = 3))
                if not blocked then
                    yield { Row = r; Col = c } ]

let applyMove (state: GameState) (from: Pos) (dest: Pos) : GameState * GameResult =
    let b = state.Board
    let mover = (getCell b from).Value
    let target = getCell b dest
    let player = state.CurrentPlayer
    let opp = opponent player

    let captured = target |> Option.map (fun p -> demote p.Kind)

    let lionCaptured =
        match target with
        | Some { Kind = Lion } -> true
        | _ -> false

    let b1 = setCell b from None
    let promote =
        match mover.Kind with
        | Chick when (player = P1 && dest.Row = 0) -> Hen
        | Chick when (player = P2 && dest.Row = 3) -> Hen
        | k -> k
    let b2 = setCell b1 dest (Some { mover with Kind = promote })

    let newCapturedP1, newCapturedP2 =
        match captured with
        | Some k when player = P1 -> k :: state.CapturedByP1, state.CapturedByP2
        | Some k when player = P2 -> state.CapturedByP1, k :: state.CapturedByP2
        | _ -> state.CapturedByP1, state.CapturedByP2

    if lionCaptured then
        let newState = { state with Board = b2; CapturedByP1 = newCapturedP1; CapturedByP2 = newCapturedP2; PendingLionWin = None }
        newState, Winner player
    else
        let newPending =
            match mover.Kind with
            | Lion when dest.Row = oppZoneRow player -> Some (player, dest)
            | _ -> None

        let newState = {
            state with
                Board = b2
                CurrentPlayer = opp
                CapturedByP1 = newCapturedP1
                CapturedByP2 = newCapturedP2
                PendingLionWin = newPending
        }
        newState, Ongoing

let applyDrop (state: GameState) (kind: PieceKind) (dest: Pos) : GameState =
    let player = state.CurrentPlayer
    let opp = opponent player
    let b = setCell state.Board dest (Some { Kind = kind; Owner = player })
    let newCapturedP1, newCapturedP2 =
        match player with
        | P1 ->
            let lst = state.CapturedByP1
            let idx = lst |> List.findIndex (fun k -> k = kind)
            lst |> List.indexed |> List.filter (fun (i,_) -> i <> idx) |> List.map snd,
            state.CapturedByP2
        | P2 ->
            state.CapturedByP1,
            let lst = state.CapturedByP2
            let idx = lst |> List.findIndex (fun k -> k = kind)
            lst |> List.indexed |> List.filter (fun (i,_) -> i <> idx) |> List.map snd
    { state with
        Board = b
        CurrentPlayer = opp
        CapturedByP1 = newCapturedP1
        CapturedByP2 = newCapturedP2
        PendingLionWin = state.PendingLionWin 
    }

let checkPendingLionWin (prevPending: (Player * Pos) option) (state: GameState) : GameResult =
    match prevPending with
    | Some (player, pos) when player <> state.CurrentPlayer ->
        match getCell state.Board pos with
        | Some { Kind = Lion; Owner = p } when p = player ->
            Winner player
        | _ -> Ongoing
    | _ -> Ongoing