module List where

data List a = Empty | Cons a (List a)

-- toList :: [a] -> List a
-- toList [] = Empty
-- toList (x:xs) = Cons x (toList xs)
--
-- fromList :: List a -> [a]
-- fromList Empty = []
-- fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Empty

fromList :: List a -> [a]
fromList = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) = f x $ listFoldr f b xs 

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) = listFoldl f (f b x) xs 

listHead :: List a -> Maybe a
listHead (Cons x xs) = Just x
listHead _ = Nothing

listTail :: List a -> List a
listTail (Cons x xs) = xs
listTail Empty = Empty

listReverse :: List a -> List a
listReverse = listFoldl rcons Empty 
    where rcons x y = Cons y x 

listMap :: (a -> b) -> List a -> List b
listMap f Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)
