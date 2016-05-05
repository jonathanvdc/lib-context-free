namespace libcontextfree

type Direction = Left | Right | Stay


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


type SubroutineTuringMachine<'Q, 'Γ when 'Q : comparison and 'Γ : comparison> =
    | SubroutineTuringMachine of Map<'Q * 'Γ, 'Q * 'Γ * Direction> * 'Q * 'Γ * Set<'Q> * Map<'Q, string>

    member tm.δ : Map<'Q * 'Γ, 'Q * 'Γ * Direction> =
        match tm with
        | SubroutineTuringMachine(δ, _, _, _, _) -> δ

    member tm.q0 : 'Q =
        match tm with
        | SubroutineTuringMachine(_, q0, _, _, _) -> q0

    member tm.B : 'Γ =
        match tm with
        | SubroutineTuringMachine(_, _, B, _, _) -> B

    member tm.F : Set<'Q> =
        match tm with
        | SubroutineTuringMachine(_, _, _, F, _) -> F

    member tm.sub : Map<'Q, string> =
        match tm with
        | SubroutineTuringMachine(_, _, _, _, sub) -> sub


type SubroutineProgram<'Q, 'Γ when 'Q : comparison and 'Γ : comparison> =
    | SubroutineProgram of Map<string, SubroutineTuringMachine<'Q, 'Γ>> * string

    member p.sub : Map<string, SubroutineTuringMachine<'Q, 'Γ>> =
        match p with
        | SubroutineProgram(sub, _) -> sub

    member p.main : string =
        match p with
        | SubroutineProgram(_, main) -> main


type MultitapeTuringMachine<'Q, 'Γ when 'Q : comparison and 'Γ : comparison> =
    | MultitapeTuringMachine of Map<'Q * 'Γ list, 'Q * 'Γ * Direction * int> * 'Q * 'Γ * Set<'Q>

    member tm.δ : Map<'Q * 'Γ list, 'Q * 'Γ * Direction * int> =
        match tm with
        | MultitapeTuringMachine(δ, _, _, _) -> δ

    member tm.q0 : 'Q =
        match tm with
        | MultitapeTuringMachine(_, q0, _, _) -> q0

    member tm.B : 'Γ =
        match tm with
        | MultitapeTuringMachine(_, _, B, _) -> B

    member tm.F : Set<'Q> =
        match tm with
        | MultitapeTuringMachine(_, _, _, F) -> F


module TuringMachine =
    /// Advance the Turing machine by one step, or return a result if we're done.
    let rec runStep (tm : TuringMachine<'Q, 'Γ>)
                    (q : 'Q)
                    (tape : Map<int, 'Γ>)
                    (pos : int) : bool =
        // Debug with:
        // for i in 0..10 do
        //     let sym = (defaultArg (Map.tryFind i tape) tm.B)
        //     if sym = tm.B then
        //         System.Console.ForegroundColor <- System.ConsoleColor.Blue
        //     if pos = i then
        //         System.Console.ForegroundColor <- System.ConsoleColor.Yellow
        //     printf "%s%O " (if pos = i then ">" else " ") sym
        //     System.Console.ResetColor()
        // printf "\n"

        if tm.F.Contains q then
            true
        else
            let X : 'Γ = defaultArg (Map.tryFind pos tape) tm.B
            match Map.tryFind (q, X) tm.δ with
                | Some (p, Y, dir) ->
                    let tape' = Map.add pos Y tape
                    let pos' =
                        match dir with
                        | Left -> pos - 1
                        | Stay -> pos
                        | Right -> pos + 1
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


    let rec runSubroutineStep (p : SubroutineProgram<'Q, 'Γ>)
                              (tm : SubroutineTuringMachine<'Q, 'Γ>)
                              (q : 'Q)
                              (tape : Map<int, 'Γ>)
                              (pos : int) : ((Map<int, 'Γ> * int) * bool) =
        // Debug with:
        for i in 0..10 do
            let sym = (defaultArg (Map.tryFind i tape) tm.B)
            if sym = tm.B then
                System.Console.ForegroundColor <- System.ConsoleColor.Blue
            if pos = i then
                System.Console.ForegroundColor <- System.ConsoleColor.Yellow
            printf "%s%O " (if pos = i then ">" else " ") sym
            System.Console.ResetColor()
        printf "\n"

        if tm.F.Contains q then
            ((tape, pos), true)
        else
            let (tape', pos') =
                match Map.tryFind q tm.sub with
                | Some name ->
                    let subTM = Map.find name p.sub
                    fst (runSubroutineStep p subTM subTM.q0 tape pos)
                | None ->
                    (tape, pos)

            let X : 'Γ = defaultArg (Map.tryFind pos' tape') tm.B
            match Map.tryFind (q, X) tm.δ with
            | Some (r, Y, dir) ->
                let tape'' = Map.add pos' Y tape'
                let pos'' =
                    match dir with
                    | Left -> pos' - 1
                    | Stay -> pos'
                    | Right -> pos' + 1
                runSubroutineStep p tm r tape'' pos''
            | None ->
                ((tape, pos), false)


    /// Run a Turing machine with subroutines over some input string,
    /// and return whether or not the TM accepts this string.
    let runSubroutineProgram (p : SubroutineProgram<'Q, 'Γ>) (input : 'Γ list) : bool =
        let initialTape =
            input |> List.mapi (fun i sym -> (i, sym))
                  |> Map.ofList

        let mainTM = Map.find p.main p.sub

        snd (runSubroutineStep p mainTM mainTM.q0 initialTape 0)


    type MultitapeSymbol<'Γ> =
        | LeftEndMarker
        | OldState of 'Γ
        | MultiState of ('Γ * bool) list

    let multitapeToSubroutine (mtm : MultitapeTuringMachine<'Q, 'Γ>) : SubroutineProgram<int, MultitapeSymbol<'Γ>> =
        