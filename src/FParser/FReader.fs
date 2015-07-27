module FReader

open System.IO

let ReadLineDelimited (reader:StreamReader) (textQualifier:char) =
    let rec lineHasUnclosedTextQualifier (textQualifier:char) (line:string) (textQualifierIsOpened:bool) =
        match line.Length with
        | 0 ->
            textQualifierIsOpened
        | _ ->
            let first = line.[0]
            lineHasUnclosedTextQualifier
                textQualifier line.[1..line.Length - 1]
                (if first = textQualifier then not textQualifierIsOpened else textQualifierIsOpened)

    let rec readUntilFinished (reader:StreamReader) (textQualifier:char) (currentLine:string) =
        let line = currentLine + reader.ReadLine()
        match (reader.Peek() > -1 && lineHasUnclosedTextQualifier textQualifier line false) with
        | false ->
            line
        | true ->
            readUntilFinished reader textQualifier (if reader.Peek() > -1 then line + "\n" else "")

    readUntilFinished reader textQualifier ""