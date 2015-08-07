module DelimitedStringParser

open System

type TextOption = Required | Banned | Optional
type FileField = { TextQualifier:TextOption }

type ValidationResult = Success | Failed
type ValidationFailureReason = MissingRequiredTextQualifier | MissingClosingTextQualifier | TextQualifierNotEscaped | ContainsBannedTextQualifier | NotEnoughColumns | TooManyColumns | TextQualifierUnescapedOrMissingDelimiter | None
type FileFieldValidationResult = { Result:ValidationResult; FailureReason:ValidationFailureReason; FieldNumber:int }

let ValidateCharacterFromDelimitedString (delimiter:char) (textQualifier:char) (textQualifierOption:TextOption) (textOpened:bool) (previous:char option) (current:char) (next:char option) =
    let previousIsDelimiter = previous = Some delimiter
    let previousIsTextQualifier = previous = Some textQualifier
    let previousIsNone = previous = Option.None
    let currentIsTextQualifier = current = textQualifier
    let nextIsTextQualifier = next = Some textQualifier
    let nextIsDelimiter = next = Some delimiter
    let nextIsNone = next = Option.None
    match (textOpened, nextIsNone, currentIsTextQualifier, previousIsDelimiter) with
    | (true, true, false, _) -> MissingClosingTextQualifier
    | (_, false, false, false) -> None
    | (_, _, _, _) ->
        match (textQualifierOption, textOpened, previousIsDelimiter, previousIsTextQualifier, previousIsNone, currentIsTextQualifier, nextIsTextQualifier, nextIsDelimiter, nextIsNone) with
        | (Required, _, false, false, true, false, _, _, _) -> MissingRequiredTextQualifier
        | (Required, false, true, _, _, false, _, _, _) -> MissingRequiredTextQualifier
        | (Banned, _, false, false, true, true, _, _, _) -> ContainsBannedTextQualifier
        | (Banned, false, true, _, _, true, false, _, _) -> ContainsBannedTextQualifier
        | (_, true, _, _, _, false, false, false, true) -> MissingClosingTextQualifier
        | (_, false, false, false, _, true, false, _, _) -> TextQualifierNotEscaped
        | (_, true, _, _, _, true, false, false, false) -> TextQualifierNotEscaped
        | (_, _, _, _, _, _, _, _, _) -> None

let TestableValidateDelimitedString (delimiter:char) (textQualifier:char) (value:string) (fields:list<FileField>) validateCharacterFunc =
    let rec validateFields value delimiter textQualifier (fields:list<FileField>) currentField textOpened (previous:char option) =
        let failResult = { Result = Failed; FailureReason = None; FieldNumber = 0; }
        
        let nextCharacter (value:string) =
            if value.Length > 1 then Some(value.[1]) else Option.None
        
        match currentField > fields.Length with
        | true -> { failResult with FailureReason = TooManyColumns; }
        | false ->
            match value with
            | "" ->
                match currentField < fields.Length with
                | true -> { failResult with FailureReason = NotEnoughColumns; }
                | false -> { Result = Success; FailureReason = None; FieldNumber = 0 }
            | _ ->
                let current = value.[0]
                let next = nextCharacter value
                let textQualifierOption = fields.[currentField-1].TextQualifier

                let characterValidationResult = validateCharacterFunc delimiter textQualifier textQualifierOption textOpened previous current next
                    
                match characterValidationResult = None with
                | false -> { failResult with FailureReason = characterValidationResult; FieldNumber = currentField }
                | true ->
                    let isNewField = current = delimiter && not textOpened

                    validateFields (value.[1..value.Length-1])
                        delimiter
                        textQualifier
                        fields
                        (if isNewField then (currentField + 1) else currentField)
                        (if current = textQualifier then not textOpened else textOpened)
                        (Some current)
                    

    validateFields value delimiter textQualifier fields 1 false Option.None

let ValidateDelimitedString (delimiter:char) (textQualifier:char) (value:string) (fields:list<FileField>) =
    TestableValidateDelimitedString delimiter textQualifier value fields ValidateCharacterFromDelimitedString