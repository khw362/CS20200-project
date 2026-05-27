module AnimalShogi.Program

open Board
open Display
open Game

[<EntryPoint>]
let main _ =
    printfn "=============================="
    printfn "         Animal Shogi         "
    printfn "=============================="
    printfn ""
    printfn "  Player 1 vs Player 2  "
    printfn ""
    printHelp ()
    let state = initialState ()
    gameLoop state
    0
