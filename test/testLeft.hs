type LeftList = [a] -> [a]

getLeftList l = l []

sample1, sample2, sample3 :: LeftList
sample1 = \l -> l ++ "some"
sample2 = \l -> l ++ "other"
sample3 = \l -> l ++ "else"

sample23 = \l -> sample3 (sample2 l)

sample123 = \l -> sample23 (sample1 l)

{-

sample123
-> \l -> sample23 (sample1 l)
-> \l -> sample23 (l ++ "some")
-> \l -> sample3 (sample2 (l ++ "some"))
-> \l -> sample3 ((l ++ "some") ++ "other)
-> \l -> ((l ++ "some") ++ "other") ++ "else"

-}
