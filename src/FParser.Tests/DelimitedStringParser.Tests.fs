module DelimitedStringParser

open FsUnit
open NUnit.Framework

open DelimitedStringParser

let validateString = TestableValidateDelimitedString ',' '"'
let success = { Result = Success; FailureReason = None; FieldNumber = 0 }

let failure = { Result = Failed; FailureReason = None; FieldNumber = 0 }
let failureTooMany = { failure with FailureReason = TooManyColumns }
let failureNotEnough = { failure with FailureReason = NotEnoughColumns }
let failureMissingTextQualifier fieldNumber = 
    { failure with FailureReason = MissingRequiredTextQualifier; FieldNumber = fieldNumber }

let validateCharacterSuccessFunc = fun (a:char) (b:char) (c:TextOption) (d:bool) (e:char option) (f:char) (g:char option) -> None

[<Test>]
let ``ValidateCharacterFromDelimitedString should succeed with valid parameters`` () =
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some 'a') 'b' (Some 'c') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some 'a') 'b' (Some 'c') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some '"') '"' (Some '"') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some '"') ',' (Some 'g') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some '"') '"' (Some '"') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some 'a') '"' (Some '"') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some '"') '"' (Some 'a') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some 'g') '"' (Some '"') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some ',') '"' (Some '"') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some '"') '"' (Some ',') |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some ',') '"' Option.None |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some '"') ',' Option.None |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Banned false Option.None ',' Option.None |> should equal None
    ValidateCharacterFromDelimitedString ',' '"' Banned false (Some ',') '"' (Some '"') |> should equal None

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with TextQualifierNotEscaped when the current character is an unescaped text qualifier`` () =
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some ',') '"' (Some 'g') |> should equal TextQualifierNotEscaped
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some 'g') '"' (Some 'g') |> should equal TextQualifierNotEscaped

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with MissingClosingTextQualifier when text is open and it's the last character and is not the textQualifier`` () =
    ValidateCharacterFromDelimitedString ',' '"' Required true (Some ',') ',' Option.None
    |> should equal (MissingClosingTextQualifier)

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with ContainsBannedTextQualifier when text is not open and previous character is delimiter and text qualifier is banned and present and next character does not escape it`` () =
    ValidateCharacterFromDelimitedString ',' '"' Banned false (Some ',') '"' (Some 'g')
    |> should equal (ContainsBannedTextQualifier)

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with MissingRequiredTextQualifier when text is not open and previous character is delimiter and text qualifier is required and missing`` () =
    ValidateCharacterFromDelimitedString ',' '"' Required false (Some ',') 'f' (Some 'g')
    |> should equal (MissingRequiredTextQualifier)

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with MissingRequiredTextQualifier when there is no previous character and text qualifier is required and missing`` () =
    ValidateCharacterFromDelimitedString ',' '"' Required false Option.None ',' Option.None
    |> should equal (MissingRequiredTextQualifier)

[<Test>]
let ``ValidateCharacterFromDelimitedString should fail with ContainsBannedTextQualifier when there is no previous character and text qualifier is banned and exists`` () =
    ValidateCharacterFromDelimitedString ',' '"' Banned false Option.None '"' Option.None
    |> should equal (ContainsBannedTextQualifier)

[<Test>]
let ``ValidateDelimitedString should fail when second field has a failing character validation`` () =
    validateString
        "\"\",foo,"
        [ { TextQualifier = Required }; { TextQualifier = Required }; { TextQualifier = Required } ]
        (fun (a) (b) (textQualifierOption) (textOpened) (previous) (current) (next) ->
            match (textQualifierOption, textOpened, previous = Some ',', current = 'f', next = Some 'o') with
            | (Required, false, true, true, true) -> MissingRequiredTextQualifier
            | (_, _, _, _, _) -> None
        )
    |> should equal (failureMissingTextQualifier 2)

[<Test>]
let ``ValidateDelimitedString should fail when first field has a failing character validation`` () =
    validateString
         ",foo,"
         [ { TextQualifier = Required }; { TextQualifier = Required }; { TextQualifier = Required } ]
         (fun (a) (b) (textQualifierOption) (textOpened) (previous) (current) (next) ->
            match (textQualifierOption, textOpened, previous = Option.None, current = ',', next = Some 'f') with
            | (Required, false, true, true, true) -> MissingRequiredTextQualifier
            | (_, _, _, _, _) -> None
         )
    |> should equal (failureMissingTextQualifier 1)

[<Test>]
let ``ValidateDelimitedString with basic fields`` () =
    let r1 = validateString ",foo," [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    r1 |> should equal failureTooMany
    let r2 = validateString ",foo," [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    r2 |> should equal failureTooMany
    let r3 = validateString ",foo," [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    r3|> should equal success

[<Test>]
let ``ValidateDelimitedString with basic quoted field`` () =
    validateString ",\"foo\"," [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"foo\"," [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"foo\"," [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal success

[<Test>]
let ``ValidateDelimitedString with embedded newline`` () =
    validateString ",\"foo\nbar\"" [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"foo\nbar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal success
    validateString ",\"foo\nbar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureNotEnough

[<Test>]
let ``ValidateDelimitedString with embedded delimiter`` () =
    validateString ",\"foo,bar\"" [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"foo,bar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal success
    validateString ",\"foo,bar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureNotEnough

[<Test>]
let ``ValidateDelimitedString with embedded text qualifier`` () =
    validateString ",\"foo\"\"bar\"" [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"foo\"\"bar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal success
    validateString ",\"foo\"\"bar\"" [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureNotEnough

[<Test>]
let ``ValidateDelimitedString with with empty string`` () =
    validateString ",\"\"," [ { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"\"," [ { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal failureTooMany
    validateString ",\"\"," [ { TextQualifier = Optional }; { TextQualifier = Optional }; { TextQualifier = Optional } ] validateCharacterSuccessFunc
    |> should equal success