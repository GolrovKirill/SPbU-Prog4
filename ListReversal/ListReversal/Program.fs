let revers list = 
    if List.isEmpty list then
        []
    else
        let rec reversList firstList secondList =
            if List.isEmpty firstList then
                secondList
            else reversList (List.tail firstList) ((List.head firstList) :: secondList)

        reversList list []

let list = [1; 2; 3; 4]
let reversList = revers list

for element in reversList do
    printf "%d " element


