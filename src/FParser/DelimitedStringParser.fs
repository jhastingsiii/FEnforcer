module DelimitedStringParser

let ValidateDelimitedString (delimiter:char) (textQualifier:char) (value:string) (columnCount:int) =
    let rec countFields value delimiter textQualifier increment textOpened =
        if increment > columnCount then increment
        else match value with
        | "" ->
            increment+1
        | _ ->
            let first = value.[0]
            countFields (value.[1..value.Length-1])
                delimiter
                textQualifier
                (if first = delimiter && not textOpened then increment+1 else increment)
                (if first = textQualifier then not textOpened else textOpened)
    
    countFields value delimiter textQualifier 0 false = columnCount