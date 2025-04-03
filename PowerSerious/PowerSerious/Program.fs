let revers list = 
   if List.isEmpty list then
        []
    else
        let rec reversList firstList secondList =
            if List.isEmpty firstList then
                secondList
            else reversList (List.tail firstList) ((List.head firstList) :: secondList)

        reversList list []

let listPowerNumber n m =
    let rec listPower i m num list =
        if i = m then
            (num * 2) :: list
        else listPower (i + 1) m (num * 2) (num :: list)

    let num = 1 <<< n
    let listResult = listPower 0 m num []
    let result = revers listResult
    result

let printList = listPowerNumber 1 3
for element in printList do
    printf $"{element} "