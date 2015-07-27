module DelimitedStringParser

open FsUnit
open NUnit.Framework
open DelimitedStringParser

let validateString = ValidateDelimitedString ',' '"'

[<Test>]
let ``ValidateDelimitedString with basic fields`` () =
    validateString ",foo," 1 |> should equal false
    validateString ",foo," 3 |> should equal true
    validateString ",foo," 2 |> should equal false

[<Test>]
let ``ValidateDelimitedString with basic quoted field`` () =
    validateString ",\"foo\"," 1 |> should equal false
    validateString ",\"foo\"," 2 |> should equal false
    validateString ",\"foo\"," 3 |> should equal true

[<Test>]
let ``ValidateDelimitedString with embedded newline`` () =
    validateString ",\"foo\nbar\"" 1 |> should equal false
    validateString ",\"foo\nbar\"" 2 |> should equal true
    validateString ",\"foo\nbar\"" 3 |> should equal false

[<Test>]
let ``ValidateDelimitedString with embedded delimiter`` () =
    validateString ",\"foo,bar\"" 1 |> should equal false
    validateString ",\"foo,bar\"" 2 |> should equal true
    validateString ",\"foo,bar\"" 3 |> should equal false

[<Test>]
let ``ValidateDelimitedString with embedded text qualifier`` () =
    validateString ",\"foo\"\"bar\"" 1 |> should equal false
    validateString ",\"foo\"\"bar\"" 2 |> should equal true
    validateString ",\"foo\"\"bar\"" 3 |> should equal false

[<Test>]
let ``ValidateDelimitedString with with empty string`` () =
    validateString ",\"\"," 1 |> should equal false
    validateString ",\"\"," 2 |> should equal false
    validateString ",\"\"," 3 |> should equal true