namespace libcontextfree

type Direction = Left | Right

type TuringMachine<'Q, 'Γ when 'Q : comparison and 'Γ : comparison> =
    | TuringMachine of Map<'Q * 'Γ, 'Q * 'Γ * Direction> * 'Q * 'Γ * Set<'Q>

    member tm.δ : Map<'Q * 'Γ, 'Q * 'Γ * Direction> =
        match tm with
        | TuringMachine(δ, _, _, _) -> δ

    member tm.q0 : 'Q =
        match tm with
        | TuringMachine(_, q0, _, _) -> q0

    member tm.B : 'Γ =
        match tm with
        | TuringMachine(_, _, B, _) -> B

    member tm.F : Set<'Q> =
        match tm with
        | TuringMachine(_, _, _, F) -> F

module TuringMachine =
    /// Advance the Turing machine by one step, or return a result if we're done.
    let rec runStep (tm : TuringMachine<'Q, 'Γ>)
                    (q : 'Q)
                    (tape : Map<int, 'Γ>)
                    (pos : int) : bool =
        // Debug with:
        // for i in 0..10 do
        //     printf "%s%A " (if pos = i then ">" else " ")
        //                    (defaultArg (Map.tryFind i tape) tm.B)
        // printf "\n"

        if tm.F.Contains q then
            true
        else
            let X : 'Γ = defaultArg (Map.tryFind pos tape) tm.B
            match Map.tryFind (q, X) tm.δ with
                | Some (p, Y, dir) ->
                    let tape' = Map.add pos Y tape
                    let pos' = if dir = Left then pos - 1 else pos + 1
                    runStep tm p tape' pos'
                | None ->
                    false
    
    /// Run a Turing machine over some input string, and return whether
    /// or not the TM accepts this string.
    let run (tm : TuringMachine<'Q, 'Γ>) (input : 'Γ list) : bool =
        let initialTape =
            input |> List.mapi (fun i sym -> (i, sym))
                  |> Map.ofList

        runStep tm tm.q0 initialTape 0