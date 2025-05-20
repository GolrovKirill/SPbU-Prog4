module PhoneDirectoryTests

open NUnit.Framework
open FsUnit
open PhoneDirectory
open System
open System.IO

let testFolder = @"C:\Users\Think\Documents\Учёба\Прога\SPbU-Prog4\PhoneDirectory\Tests"
let testFilePath = Path.Combine(testFolder, "contacts.txt")

[<TestFixture>]
type FileOperationsTests() =
    
    [<SetUp>]
    member this.Setup() =
        if Directory.Exists(testFolder) then
            Directory.Delete(testFolder, true)
        Directory.CreateDirectory(testFolder) |> ignore

    [<Test>]
    member _.``readFileWithDirectory should read valid file``() =
        File.WriteAllLines(testFilePath, [| "Si:+70304221245"; "Di:+30304221233"; "Jo:+30304221245" |])

        let result = readFileWithDirectory testFilePath
        
        match result with
        | Ok contacts ->
            contacts |> should equal [
                { Name = "Si"; Number = "+70304221245" }
                { Name = "Di"; Number = "+30304221233" }
                { Name = "Jo"; Number = "+30304221245" }
            ]
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Test>]
    member _.``readFileWithDirectory should ignore invalid lines``() =
        File.WriteAllLines(testFilePath, [| 
            "Valid:+1234567890"
            "InvalidLine"
            ":EmptyName"
            "EmptyNumber:"
            "Too:Many:Colons" 
        |])

        let result = readFileWithDirectory testFilePath
        
        match result with
        | Ok contacts ->
            contacts |> should equal [ { Name = "Valid"; Number = "+1234567890" } ]
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Test>]
    member _.``writeInFile should create file with contacts``() =
        let testData = [
            { Name = "Alice"; Number = "+1234567890" }
            { Name = "Bob"; Number = "+0987654321" }
        ]

        let result = writeInFile testData testFilePath
    
        match result with
        | Ok msg -> 
            msg |> should equal "Данные успешно сохранены"
            File.ReadAllLines(testFilePath) |> should equal [| "Alice:+1234567890"; "Bob:+0987654321" |]
        | Error e -> Assert.Fail($"Ожидался успех, но получена ошибка: {e}")

    [<Test>]
    member _.``writeInFile should create directories if not exists``() =
        let nestedPath = Path.Combine(testFolder, "nested", "contacts.txt")
        let testData = [ { Name = "Test"; Number = "+0000000000" } ]

        let result = writeInFile testData nestedPath
    
        match result with
        | Ok msg -> 
            msg |> should equal "Данные успешно сохранены"
            File.Exists(nestedPath) |> should be True
        | Error e -> Assert.Fail($"Ожидался успех, но получена ошибка: {e}")

    [<Test>]
    member _.``readFileWithDirectory should return empty list for non-existent file``() =
        let nonExistentPath = Path.Combine(testFolder, "non_existent.txt")
    
        let result = readFileWithDirectory nonExistentPath
    
        match result with
        | Ok contacts -> contacts |> should be Empty
        | Error e -> Assert.Fail($"Ожидался успех, но получена ошибка: {e}")

    [<Test>]
    member _.``readFileWithDirectory should handle file read errors``() =
        let lockedFilePath = Path.Combine(testFolder, "locked.txt")
        use fs = File.Create(lockedFilePath)
        
        let result = readFileWithDirectory lockedFilePath
        
        match result with
        | Error msg -> msg |> should startWith "The process cannot access the file"
        | Ok _ -> Assert.Fail("Expected error but got Ok")

[<EntryPoint>]
let main _ = 0

