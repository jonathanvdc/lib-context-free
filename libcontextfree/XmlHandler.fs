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

    type CFGFile = XmlProvider<CfgXmlSample>

    /// Creates a CFG XML node from the given context-free grammar.
    let ofCfg (cfg : ContextFreeGrammar<char, char>) : CFGFile.Cfg =
        let setToString (xs : Set<char>) =
            xs |> Seq.map string 
               |> String.concat ""

        let terminals = setToString cfg.T
        let nonterminals = setToString cfg.V
        let startSymbol = string cfg.S
        let variablesNode = XElement(XName.Get("Variables"))
        variablesNode.Add(nonterminals)
        variablesNode.Add(XElement(XName.Get("StartSymbol"), startSymbol))
        let variables = CFGFile.Variables(variablesNode)
        let rules = cfg.P |> Seq.map (fun (ProductionRule(head, body)) -> 
                                      CFGFile.Rule(body |> Seq.map string |> String.concat "", string head))
                          |> Array.ofSeq
        let productions = CFGFile.Productions(rules)
        CFGFile.Cfg(variables, terminals, productions)

    /// Tries to convert the given CFG XML node to a context-free grammar.
    /// If this cannot be done, None is returned.
    let toCfg (input : CFGFile.Cfg) : Result<ContextFreeGrammar<char, char>> =
        let terminals = input.Terminals |> Set.ofSeq

        let toSymbol c =
            if terminals.Contains c then
                Terminal c
            else
                Nonterminal c

        match input.Variables.StartSymbol |> Seq.toList with
        | [startSymbol] ->
            let toRule (inputRule : CFGFile.Rule) : Result<Set<ProductionRule<char, char>>> =
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