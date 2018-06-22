{-# LANGUAGE ExistentialQuantification #-}

newtype C x = C x
data D = D Int Double
data I a = forall b . I b

f :: Int -> [Int] -> Int
f x xs =
    if x == 0 then x
    else sum . map id . map (*2) $ xs

d = map f . map (map f1 . map g1)
e = ((map f)) .
      (map ((map f1) . map g1))
g = map f
      (map g xs)
i = map f $ map g xs

h = (map $ f) $ map g xs

m = map f $ fmap g xs

-- wrong
k = map f $ map g
k2 = map f $ concat xs

