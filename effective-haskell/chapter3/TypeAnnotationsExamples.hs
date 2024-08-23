module TypeAnnotationsExamples where

calculateTotalCost :: Int -> Int
calculateTotalCost basePrice = 
    let
        priceWithServiceFee :: Int
        priceWithServiceFee = basePrice + 1
        customaryTip = 7 :: Int
    in priceWithServiceFee + customaryTip 
