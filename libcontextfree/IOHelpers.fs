namespace libcontextfree

open System
open System.IO

module IOHelpers = 
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

    let wordWrap (columns : int) (text : string) : seq<string> =
        let takeLine (s : string) = s.[0..columns - 1]
        let rec wrap (s : string) = seq {
            if s.Length > columns then
                let i, j =
                    match (takeLine s).LastIndexOf(' ') with
                    | -1 -> columns, columns
                    | x -> x, x + 1

                yield s.[0..i - 1]
                yield! (wrap s.[j..])
            else
                yield s
        }
        wrap text