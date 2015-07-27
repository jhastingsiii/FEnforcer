module FReader

open NUnit.Framework
open FsUnit
open System.Reflection
open System.IO
open FReader

let GetLineFromTestFile fileName rowNumber =
    let resourceNames = Assembly.GetExecutingAssembly().GetManifestResourceNames()
    use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(fileName)
    use reader = new StreamReader(stream)
    let rec readUntilDone reader row =
        let line = ReadLineDelimited reader '"'
        if row = 1 then line else (readUntilDone reader (row-1))

    readUntilDone reader rowNumber

let GetLineFromMultiLineRowsTestFile rowNumber =
    GetLineFromTestFile "MultiLineRows.csv" rowNumber

[<Test>]
let ``ReadLineDelimited will read lines with multiline text values`` () =
    GetLineFromMultiLineRowsTestFile 2 |> should equal "t,\"t\n\nt\",t"

[<Test>]
let ``ReadLineDelimited will read lines with escaped text qualifiers`` () =
    GetLineFromMultiLineRowsTestFile 4 |> should equal "t,\"t\"\"t\"\"t\",\"\""

[<Test>]
let ``ReadLineDelimited will read lines with no text qualifiers`` () =
    GetLineFromMultiLineRowsTestFile 1 |> should equal "col1,col2,col3"
    GetLineFromMultiLineRowsTestFile 3 |> should equal "t,t,t"