module Lab6 where

data RegularPoly = Polygon Int Float

polyArea :: RegularPoly -> Float
polyArea (Polygon sides len1) = do
 let sides2 = fromIntegral sides
 ((len1^2)*sides2)/(4*(tan(180.0/sides2)))

instance Eq RegularPoly where
 x == y = polyArea x == polyArea y

instance Show RegularPoly where
 show (Polygon x y)  = "There are " ++ show x ++ " sides and they are " ++ show y ++ " in length."
