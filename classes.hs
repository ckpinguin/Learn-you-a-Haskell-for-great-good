class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

--instance YesNo a would not be clever, as we cannot determine
--False for any generic type

instance YesNo Bool where
    yesno = id

-- A parametrized type as an instance of the class
-- normally this can be dangerous, if we cannot control or assure
-- that the type really obeys the interface contract, we also put
-- a class constraint for the generic type. example:
-- instance (Eq m) => Eq (Maybe m) where...
-- But with our YesNo type, we don't do special stuff, so we 
-- don't need this
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- instance YesNo (Tree a) where
--     yesno EmptyTree = False
--     yesno _ = True

-- YesNo class can be used to make an if-construct, that
-- mimics the ?:-Operator (oneline-conditional):
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult

-- Strict eval (no lazyness)
data RealFloat a => Complex a = !a :+ !a

