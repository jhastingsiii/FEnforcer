module FReader

open System.IO

let ReadLineDelimited (reader:StreamReader) (textQualifier:char) =
    let rec lineHasUnclosedTextQualifier (textQualifier:char) (line:string) (textQualifierIsOpened:bool) =
        match line.Length with
        | 0 ->
            textQualifierIsOpened
        | _ ->
            let first = line.[0]
            let stillOpened =  (not textQualifierIsOpened && first = textQualifier) || (textQualifierIsOpened && first <> textQualifier)

            lineHasUnclosedTextQualifier textQualifier line.[1..line.Length - 1] stillOpened

    let rec readUntilFinished (reader:StreamReader) (textQualifier:char) (currentLine:string) =
        let line = currentLine + reader.ReadLine()
        match (reader.Peek() > -1 && lineHasUnclosedTextQualifier textQualifier line false) with
        | false ->
            line
        | true ->
            readUntilFinished reader textQualifier (if reader.Peek() > -1 then line + "\n" else "")

    readUntilFinished reader textQualifier ""