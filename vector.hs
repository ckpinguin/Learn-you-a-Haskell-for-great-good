data Vector a = Vector a a a deriving (Show)

-- Only use type class restrictions on functions, never with types!
vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vPlus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

svMult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `svMult` m = Vector (i*m) (j*m) (k*m)

crossProd :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `crossProd` (Vector l m n) = Vector (j*n-k*m) (k*l-i*n) (i*m-j*l)

