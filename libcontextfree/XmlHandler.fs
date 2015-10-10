namespace libcontextfree

open FSharp.Data
open System.Xml.Linq

module XmlHandler =
    [<Literal>]
    let private CfgXmlSample = 
        """<?xml version="1.0"?>
        <CFG>
            <Variables>
                SR
                <StartSymbol>S</StartSymbol>
            </Variables>
            <Terminals> ab </Terminals>
            <Productions>
                <Rule RHS="aSb|b" LHS="S"/>
                <Rule RHS="R" LHS="S"/>
                <Rule RHS="a" LHS="R"/>
            </Productions>
        </CFG>
        """

    type CfgFile = XmlProvider<CfgXmlSample>

    /// Creates a CFG XML node from the given context-free grammar.
    let ofCfg (cfg : ContextFreeGrammar<char, char>) : CfgFile.Cfg =
        let setToString (xs : Set<char>) =
            xs |> Seq.map string 
               |> String.concat ""

        let terminals = setToString cfg.T
        let nonterminals = setToString cfg.V
        let startSymbol = string cfg.S
        let variablesNode = XElement(XName.Get("Variables"))
        variablesNode.Add(nonterminals)
        variablesNode.Add(XElement(XName.Get("StartSymbol"), startSymbol))
        let variables = CfgFile.Variables(variablesNode)
        let rules = cfg.P |> Seq.map (fun (ProductionRule(head, body)) -> 
                                      CfgFile.Rule(body |> Seq.map string |> String.concat "", string head))
                          |> Array.ofSeq
        let productions = CfgFile.Productions(rules)
        CfgFile.Cfg(variables, terminals, productions)

    /// Tries to convert the given CFG XML node to a context-free grammar.
    /// If this cannot be done, an Error value is returned.
    let toCfg (input : CfgFile.Cfg) : Result<ContextFreeGrammar<char, char>> =
        let terminals = input.Terminals |> Set.ofSeq

        let toSymbol c =
            if terminals.Contains c then
                Terminal c
            else
                Nonterminal c

        match input.Variables.StartSymbol |> Seq.toList with
        | [startSymbol] ->
            let toRule (inputRule : CfgFile.Rule) : Result<Set<ProductionRule<char, char>>> =
                match inputRule.Lhs |> Seq.toList with
                | [ruleHead] -> 
                    Success (inputRule.Rhs.Split('|')
                             |> Seq.map (fun body ->
                                 let convertedBody = List.ofSeq (Seq.map toSymbol body)
                                 ProductionRule(ruleHead, convertedBody))
                             |> Set.ofSeq)
                | _ ->
                    // There can be only one head symbol per production rule.
                    // Quit if this invariant is not respected.
                    Error (sprintf "The rule '%s -> %s' in the given context-free grammar description \
                                    contains more than one head symbol." inputRule.Lhs inputRule.Rhs)

            input.Productions.Rules
            |> Array.toList
            |> List.map toRule
            |> Result.sequence
            |> Result.map (fun rs -> ContextFreeGrammar(Set.unionMany rs, startSymbol))

        | _ -> 
            Error "The given context-free grammar description contains \
                   more than one start symbol."

    [<Literal>]
    let private PdaXmlSample = 
        """<?xml version="1.0"?>
        <PDA>
            <Alphabet>
                <symbol>A</symbol>
                <symbol>B</symbol>
            </Alphabet>
            <StackAlphabet>
                <symbol>C</symbol>
                <symbol>D</symbol>
            </StackAlphabet>
            <States>
                <state>
                    <name>E</name>
                    <starting>no</starting>
                    <accepting>no</accepting>
                </state>
                <state>
                    <name>F</name>
                </state>
            </States>
            <Transitions>
                <transition>
                    <from>E</from>
                    <to>F</to>
                    <input>A</input>
                    <stacktop>C</stacktop>
                    <operation>PUSH</operation>
                    <push>D</push>
                    <push>C</push>
                </transition>
                <transition>
                    <from>F</from>
                    <to>E</to>
                    <input>B</input>
                    <stacktop>D</stacktop>
                    <operation>STAY</operation>
                </transition>
            </Transitions>
        </PDA>
        """

    type PdaFile = XmlProvider<PdaXmlSample>

    /// Creates a PDA XML node from the given pushdown automaton.
    let ofPda (pda : PushdownAutomaton<string, string, string>) : PdaFile.Pda =
        match pda with
        | PDA(Q, Σ, Γ, δ, q0, Z0, F) ->
            let alphabet : PdaFile.Alphabet =
                let syms = Set.toArray Σ |> Array.map string
                PdaFile.Alphabet syms

            let stackAlphabet : PdaFile.StackAlphabet =
                let syms = Set.toArray Γ |> Array.map string
                PdaFile.StackAlphabet syms

            let states : PdaFile.States =
                let convertState (q : string) : PdaFile.State =
                    let starting = (q = q0)
                    let accepting = Set.contains q F
                    PdaFile.State (q, Some starting, Some accepting)
                let stateArray : PdaFile.State[] =
                    Set.toArray Q |> Array.map convertState
                PdaFile.States stateArray

            let transitions : PdaFile.Transitions =
                let convertTransition (q, a, X) (p, Y) : PdaFile.Transition =
                    let input = defaultArg (Option.map string a) "empty"
                    PdaFile.Transition (q, p, input, string X, "PUSH", List.toArray Y)

                let convertTransitionGroup ((q, a, X), v) : PdaFile.Transition[] =
                    Set.toArray v |> Array.map (convertTransition (q, a, X))

                let transitionArray : PdaFile.Transition[] =
                    Map.toArray δ |> Array.collect convertTransitionGroup

                PdaFile.Transitions transitionArray
            
            PdaFile.Pda (alphabet, stackAlphabet, states, transitions)

    /// Tries to convert the given PDA XML node to a pushdown automaton.
    /// If this cannot be done, an Error value is returned.
    let toPda (input : PdaFile.Pda) : Result<PushdownAutomaton<string, string, string>> =
        let statesWhere (p : PdaFile.State -> bool) : seq<string> =
            input.States.States |> Seq.filter p
                                |> Seq.map (fun s -> s.Name)

        let F = statesWhere (fun x -> defaultArg x.Accepting false)
        let q0s = statesWhere (fun x -> defaultArg x.Starting false)

        match List.ofSeq q0s with
        | [q0] ->
            let transitions = input.Transitions.Transitions |> Array.map (fun t ->
                let a = if t.Input = "empty" then None else Some t.Input
                ((t.From, a, t.Stacktop), (t.To, Array.toList t.Pushes))
            )
            let δ : Transition<string, string, string> =
                MapHelpers.groupFstSet transitions
            Success (PushdownAutomaton (δ, q0, "Z0", Set.ofSeq F))
        | [] ->
            Error "The given PDA has no starting state."
        | q0s ->
            Error (sprintf "The given PDA has multiple starting states: %A." q0s)