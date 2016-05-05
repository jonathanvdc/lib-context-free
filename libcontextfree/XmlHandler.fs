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
            |> Seq.map toRule
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
            let transitions = input.Transitions.Transitions |> Seq.map (fun t ->
                let a : string option =
                    if t.Input = "empty" then None else Some t.Input
                let pushes : Result<string list> =
                    match t.Operation.ToLowerInvariant() with
                    | "push" -> Success (Array.toList t.Pushes)
                    | "pop"  -> Success []
                    | "stay" -> Success [t.Stacktop]
                    | op     -> Error (sprintf "Unknown transition operation: '%s'." op)
                pushes |> Result.map (fun Y ->
                    ((t.From, a, t.Stacktop), (t.To, Y))
                )
            )

            Result.sequence transitions |> Result.map (fun ts ->
                let δ = MapHelpers.groupFstSet ts
                PushdownAutomaton (δ, q0, "Z0", Set.ofSeq F))
        | [] ->
            Error "The given PDA has no starting state."
        | q0s ->
            Error (sprintf "The given PDA has multiple starting states: %A." q0s)

    
    let readDirection (d : string) : Direction =
        match d with
            | "L" -> Left
            | "R" -> Right
            | "S" -> Stay
            | _   -> raise (new System.ArgumentException("Invalid direction."))

        
    [<Literal>]
    let private TmXmlSample = 
        """<?xml version="1.0"?>
        <TM>
            <InputAlphabet>
                <symbol>0</symbol>
		        <symbol>1</symbol>
            </InputAlphabet>
            <TapeAlphabet>
                <symbol>0</symbol>
		        <symbol>1</symbol>
		        <symbol>X</symbol>
            </TapeAlphabet>
	        <Blank>B</Blank>
	        <States>
		        <state>Q0</state>
		        <state>Q1</state>
	        </States>
	        <Transitions>
		        <transition>
			        <from>Q0</from>
			        <to>Q1</to>
			        <read>Z</read>
			        <write>X</write>
			        <dir>R</dir>
		        </transition>
		        <transition>
			        <from>Q0</from>
			        <to>Q3</to>
			        <read>Y</read>
			        <write>Y</write>
			        <dir>R</dir>
		        </transition>
	        </Transitions>
	        <StartState>
		        <name>Q0</name>
	        </StartState>
	        <AcceptingStates>
		        <state>
			        <name>Q4</name>
		        </state>
                <state>
			        <name>Q5</name>
		        </state>
	        </AcceptingStates>
        </TM>
        """

    type TmFile = XmlProvider<TmXmlSample>

    let toTm (input : TmFile.Tm) : TuringMachine<string, string> =
        let readTransition (t : TmFile.Transition) : (string * string) * (string * string * Direction) =
            ((t.From, t.Read), (t.To, t.Write, readDirection t.Dir))

        let δ : Map<string * string, string * string * Direction> =
            Map.ofSeq (Seq.map readTransition input.Transitions.Transitions)

        let q0 : string =
            input.StartState.Name

        let B : string =
            input.Blank

        let F : Set<string> =
            set [ for s in input.AcceptingStates.States -> s.Name ]

        TuringMachine (δ, q0, B, F)


    [<Literal>]
    let private StmXmlSample =
        """<?xml version="1.0" ?>
        <Program main="R">
            <InputAlphabet>
                <symbol name="0"/>
                <symbol name="1"/>
            </InputAlphabet>
            <TapeAlphabet>
                <symbol name="0"/>
                <symbol name="1"/>
            </TapeAlphabet>
            <Blank>
                <symbol name="B"/>
            </Blank>
            <TM name="R">
                <States>
                    <state name="Q0"/>
                    <state name="Q1"/>
                </States>
                <Transitions>
                    <transition from="Q0" to="Q1" read="X" write="X" dir="R"/>
                    <transition from="Q0" to="Q3" read="Y" write="Y" dir="R"/>
                </Transitions>
                <StartState>
                    <state name="Q0"/>
                </StartState>
                <AcceptingStates>
                    <state name="Q4"/>
                    <state name="Q3"/>
                </AcceptingStates>
                <Subroutines>
                    <subroutine at="Q0" name="R"/>
                    <subroutine at="Q1" name="S"/>
                </Subroutines>
            </TM>
            <TM name="R">
                <States>
                    <state name="Q0"/>
                    <state name="Q1"/>
                </States>
                <Transitions>
                    <transition from="Q0" to="Q1" read="X" write="X" dir="R"/>
                    <transition from="Q0" to="Q3" read="Y" write="Y" dir="R"/>
                </Transitions>
                <StartState>
                    <state name="Q0"/>
                </StartState>
                <AcceptingStates>
                    <state name="Q4"/>
                    <state name="Q3"/>
                </AcceptingStates>
                <Subroutines>
                    <subroutine at="Q0" name="R"/>
                    <subroutine at="Q1" name="S"/>
                </Subroutines>
            </TM>
        </Program>"""

    type SubroutineProgramFile = XmlProvider<StmXmlSample>

    let toSubroutineProgram (input : SubroutineProgramFile.Program) : SubroutineProgram<string, string> =
        let readTransition (t : SubroutineProgramFile.Transition) : (string * string) * (string * string * Direction) =
                ((t.From, t.Read), (t.To, t.Write, readDirection t.Dir))

        let readTm (tm : SubroutineProgramFile.Tm) : SubroutineTuringMachine<string, string> =
            let δ : Map<string * string, string * string * Direction> =
                Map.ofSeq (Seq.map readTransition tm.Transitions.Transitions)

            let q0 : string =
                tm.StartState.State.Name

            let B : string =
                input.Blank.Symbol.Name

            let F : Set<string> =
                set [ for s in tm.AcceptingStates.States -> s.Name ]

            let sub : Map<string, string> =
                Map.ofSeq [ for s in tm.Subroutines.Subroutines -> s.At, s.Name ]

            SubroutineTuringMachine (δ, q0, B, F, sub)

        let sub : Map<string, SubroutineTuringMachine<string, string>> =
            Map.ofSeq [ for tm in input.Tms -> (tm.Name, readTm tm) ]

        SubroutineProgram (sub, input.Main)

    [<Literal>]
    let private MtmXmlSample = """<?xml version="1.0"?>
        <MTM tapes="2">
          <InputAlphabet>
            <symbol name="x"/>
            <symbol name="y"/>
          </InputAlphabet>
          <TapeAlphabet>
            <symbol name="x"/>
            <symbol name="y"/>
            <symbol name="B"/>
          </TapeAlphabet>
          <Blank>
            <symbol name="B"/>
          </Blank>
          <States>
            <state name="Q1"/>
            <state name="Q2"/>
          </States>
          <Transitions>
            <transition from="Q1" to="Q2" tape="1" write="y" dir="R">
              <reads>
                <read symbol="x"/>
                <read symbol="y"/>
              </reads>
            </transition>
            <transition from="Q1" to="Q2" tape="1" write="y" dir="R">
              <reads>
                <read symbol="x"/>
                <read symbol="y"/>
              </reads>
            </transition>
          </Transitions>
          <StartState>
            <state name="Q1"/>
          </StartState>
          <AcceptingStates>
            <state name="Q1"/>
            <state name="Q2"/>
          </AcceptingStates>
        </MTM>"""

    type MtmFile = XmlProvider<MtmXmlSample>

    let toMtm (mtm : MtmFile.Mtm) : MultitapeTuringMachine<string, string> =
        let readTransition (t : MtmFile.Transition) : (string * string list) * (string * string * Direction * int) =
            ((t.From, [ for r in t.Reads -> r.Symbol ]),
             (t.To, t.Write, readDirection t.Dir, t.Tape))

        let δ : Map<string * string list, string * string * Direction * int> =
            Map.ofSeq (Seq.map readTransition mtm.Transitions.Transitions)

        let q0 : string =
            mtm.StartState.State.Name

        let B : string =
            mtm.Blank.Symbol.Name

        let F : Set<string> =
            set [ for q in mtm.AcceptingStates.States -> q.Name ]

        MultitapeTuringMachine (δ, q0, B, F)