data Cat = Cat String
data Dog = Dog String

class Who a where
    yourname :: a -> String

instance Who Cat where
    yourname (Cat name) = name
                        
instance Who Dog where
    yourname (Dog name) = name