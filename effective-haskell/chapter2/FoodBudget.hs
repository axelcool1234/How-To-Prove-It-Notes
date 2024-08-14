module FoodBudget where

foodCosts =
    [("Ren", 10.00)
    ,("George", 4.00)
    ,("Porter", 27.50)]

checkGuestList guestList name = 
    name `elem` guestList

partyBudget isAttending = 
    foldr ((+) . snd) 0 . filter (isAttending . fst) 
