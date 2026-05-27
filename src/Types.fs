module AnimalShogi.Types

type Player = P1 | P2

type PieceKind =
    | Lion
    | Giraffe
    | Elephant
    | Chick
    | Hen

type Piece = { Kind: PieceKind; Owner: Player }

type Pos = { Row: int; Col: int }

type Cell = Piece option

type Board = Cell[,]

type GameState = {
    Board: Board
    CurrentPlayer: Player
    CapturedByP1: PieceKind list
    CapturedByP2: PieceKind list
    PendingLionWin: (Player * Pos) option
}

type MoveTarget =
    | BoardMove of fromPos: Pos * toPos: Pos
    | Drop of kind: PieceKind * toPos: Pos

type GameResult =
    | Winner of Player
    | Ongoing
