open PhoneDirectory
open System

let rec mainLoop directory =
    printfn "Выберите команду: 
            1 - выход;
            2 - добавить запись;
            3 - найти телефон по имени;
            4 - найти имя по телефону;
            5 - показать все записи;
            6 - сохранить в файл;
            7 - загрузить из файла."

    match Console.ReadLine() with
    | "1" -> 
        printfn "Выход из программы"
        ()

    | "2" ->
        printf "Введите имя: "
        let name = Console.ReadLine()
        printf "Введите телефон: "
        let number = Console.ReadLine()
        let newDirectory = addContact directory name number
        printfn "Контакт добавлен\n"
        mainLoop newDirectory

    | "3" ->
        printf "Введите имя для поиска: "
        let name = Console.ReadLine()
        match searchNumber directory name with
        | Some num -> printfn "Телефон: %s\n" num
        | None -> printfn "Контакт не найден\n"
        mainLoop directory

    | "4" ->
        printf "Введите телефон для поиска: "
        let number = Console.ReadLine()
        match searchName directory number with
        | Some numberUser -> printfn "Имя: %s\n" numberUser
        | None -> printfn "Контакт не найден\n"
        mainLoop directory

    | "5" ->
        printfn "Текущие контакты:"
        directory 
        |> getAllElements 
        |> Seq.iter (printfn "%s")
        printfn ""
        mainLoop directory

    | "6" ->
        printf "Введите путь для сохранения: "
        let path = Console.ReadLine()
        match writeInFile directory path with
        | Ok msg -> printfn $"{msg}\n"
        | Error e -> printfn $"Ошибка сохранения: {e}\n"
        mainLoop directory

    | "7" ->
        printf "Введите путь к файлу: "
        let path = Console.ReadLine()
        match readFileWithDirectory path with
        | Ok newDirectory -> 
            printfn "Данные загружены из файла\n"
            mainLoop newDirectory
        | Error e -> 
            printfn $"Ошибка загрузки: {e}\n"
            mainLoop directory

    | _ ->
        printfn "Неверная команда, попробуйте снова\n"
        mainLoop directory

[<EntryPoint>]
let main argv =
    mainLoop createDirectory
    0