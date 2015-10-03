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