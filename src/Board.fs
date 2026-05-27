module AnimalShogi.Board

open Types

let initialBoard () : Board =
    let b = Array2D.create 4 3 None
    b.[0, 1] <- Some { Kind = Lion;     Owner = P2 }
    b.[0, 0] <- Some { Kind = Giraffe;  Owner = P2 }
    b.[0, 2] <- Some { Kind = Elephant; Owner = P2 }
    b.[1, 1] <- Some { Kind = Chick;    Owner = P2 }
    b.[3, 1] <- Some { Kind = Lion;     Owner = P1 }
    b.[3, 2] <- Some { Kind = Giraffe;  Owner = P1 }
    b.[3, 0] <- Some { Kind = Elephant; Owner = P1 }
    b.[2, 1] <- Some { Kind = Chick;    Owner = P1 }
    b

let initialState () : GameState = {
    Board = initialBoard ()
    CurrentPlayer = P1
    CapturedByP1 = []
    CapturedByP2 = []
    PendingLionWin = None
}

let inBounds r c = r >= 0 && r <= 3 && c >= 0 && c <= 2

let getCell (b: Board) (p: Pos) = b.[p.Row, p.Col]

let setCell (b: Board) (p: Pos) v =
    let nb = Array2D.copy b
    nb.[p.Row, p.Col] <- v
    nb

let opponent = function P1 -> P2 | P2 -> P1

let oppZoneRow = function P1 -> 0 | P2 -> 3

let ownZoneRow = function P1 -> 3 | P2 -> 0

let demote k = match k with Hen -> Chick | x -> x
