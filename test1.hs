data A = A Int
data B x = B x
data E a b c = E a
    deriving (Show)

data Fi a b c = Fi {field :: a}
    deriving (Show)

a = map f . map g
e = ((map f)) .
      (map ((map f1) . map g1))
g = map f
      (map g xs)
h = map f $ map g $ xs ++ ys
