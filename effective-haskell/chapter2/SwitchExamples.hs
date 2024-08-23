module SwitchExamples where

favoriteFood person = 
    case person of
        "Ren" -> "Tofu"
        "Rebecca" -> "Falafel"
        "George" -> "Banana"
        name -> "I don't know what " <> name <> " likes!"

handleNums l = 
    case l of
        [] -> "An empty list"
        [x] | x == 0 -> "a list called: [0]"
            | x == 1 -> "a singular list of [1]"
            | even x -> "a singleton list containing an even number"
            | otherwise -> "the list contains " <> show x
        _list -> "the list has more than 1 element"
