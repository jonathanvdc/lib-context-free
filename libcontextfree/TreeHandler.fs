namespace libcontextfree

open System
open System.IO
open ParseTreeModule

module TreeHandler =
    /// Reads a single character from the given text reader.
    let readChar (reader : TextReader) : char option =
        let c = reader.Read()
        if c < 0 then
            None // End of stream
        else
            Some (char c)

    /// Peeks a character from the given text reader, and determines whether
    /// it satisfies the given predicate.
    let peekSatisfies (pred : char -> bool) (reader : TextReader) : bool =
        let c = reader.Peek()
        c >= 0 && pred (char c)

    /// Reads a single character from the given text reader only if it
    /// satisfies the given predicate.
    let readConditional (pred : char -> bool) (reader : TextReader) : char option =
        if peekSatisfies pred reader then
            readChar reader
        else
            None

    /// Checks if the given character belongs to a word in a parse tree file.
    let isAtomChar (c : char) : bool = 
        c <> '(' && c <> ')' && not(Char.IsWhiteSpace(c))

    /// Reads a word as a list of characters.
    let rec readAtomAsList (reader : TextReader) : char list =
        match readConditional isAtomChar reader with
        | Some '\\' -> List.append (readChar reader |> Option.toList) (readAtomAsList reader)
        | Some c    -> c :: readAtomAsList reader
        | None      -> []

    /// Reads a word as a string.
    let readAtom (reader : TextReader) : string =
        new System.String(readAtomAsList reader |> Array.ofList)

    /// Skips all leading whitespace in the text reader.
    let rec skipWhitespace (reader : TextReader) : unit =
        if peekSatisfies Char.IsWhiteSpace reader then
            ignore(readChar reader)
            skipWhitespace reader
        else
            ()

    /// Reads a parse tree node from the given text reader.
    let rec readNode (reader : TextReader) : ParseTree<string, string> option =
        skipWhitespace reader
        match readConditional ((=) '(') reader with
        | Some _ -> 
            skipWhitespace reader
            match readConditional ((=) ')') reader with
            | Some _ -> 
                // Epsilon node syntax: ()
                Some EpsilonLeaf
            | None   ->
                // Production node syntax: (<nonterminal> <nodes...>)
                let nonterm = readAtom reader
                let children = readNodes reader
                ignore(readChar reader)
                Some(ProductionNode(nonterm, children))
        | None -> 
            if peekSatisfies isAtomChar reader then
                // Terminal node syntax: <terminal>
                let terminal = readAtom reader
                Some (TerminalLeaf terminal)
            else
                None

    /// Reads a list of parse tree nodes from the given text reader.
    and readNodes (reader : TextReader) =
        match readNode reader with
        | Some node -> node :: readNodes(reader)
        | None      -> []
