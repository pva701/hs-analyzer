-- data A = A Int
-- data B x = B x
-- newtype C x = C x

-- f :: Int -> [Int] -> Int
-- f x xs =
--     if x == 0 then x
--     else sum . map id . map (*2) $ xs

a = map f . map g
b = map f (map g xs)
c = map f $ map g xs
