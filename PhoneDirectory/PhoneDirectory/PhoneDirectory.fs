module PhoneDirectory

open System
open System.IO

// Represents a contact with a name and phone number
type ElementDirectory =
    { Name: string
      Number: string }

// Initializes an empty phone directory
let createDirectory = []

// Adds a new contact to the directory
let addContact directory name number = 
    { Name = name; Number = number } :: directory

// Searches for a phone number by contact name
let searchNumber directory name =
    directory |> List.tryFind (fun x -> x.Name = name) |> Option.map (fun x -> x.Number)

// Searches for a contact name by phone number
let searchName directory number =
    directory |> List.tryFind (fun x -> x.Number = number) |> Option.map (fun x -> x.Name)

// Formats all contacts as "Name:Number" strings
let getAllElements directory = 
    directory |> List.map (fun x -> $"{x.Name}:{x.Number}")

// Writes the contacts to a file, creating directories if needed
// Returns a result indicating success or error message
let writeInFile directory path =
    try
        let fullPath = 
            if Directory.Exists(path) then
                Path.Combine(path, "contacts.txt")
            else
                path

        let dir = Path.GetDirectoryName(fullPath)
        if not (String.IsNullOrEmpty(dir)) && not (Directory.Exists(dir)) then
            Directory.CreateDirectory(dir) |> ignore

        File.WriteAllLines(fullPath, getAllElements directory)
        Ok "Данные успешно сохранены"
    with
    | ex -> Error ex.Message

// Reads contacts from a file and returns them as a directory or an error
let readFileWithDirectory path =
    try
        if File.Exists path then
            File.ReadAllLines path
            |> Array.choose (fun line ->
                match line.Split(':', StringSplitOptions.RemoveEmptyEntries) with
                | [| name; number |] -> Some { Name = name; Number = number }
                | _ -> None)
            |> Array.toList
            |> Ok
        else
            Ok []
    with
    | ex -> Error ex.Message