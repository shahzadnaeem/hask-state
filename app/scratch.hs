import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> ((Food,Food), Price)
addDrink all@"beans" = ((all,"milk"), Sum 10)
addDrink all@"bread" = ((all,"water"), Sum 0)
addDrink all@"jerky" = ((all,"whiskey"), Sum 99)
addDrink other = ((other,"beer"), Sum 30)

addItem :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
addItem (food,logIn) f = let (y, logItem) = f food in (y, logIn `mappend` logItem)

-- Try
-- ("stew", Sum 100) `addItem` addDrink
-- ("jerky", Sum 100) `addItem` addDrink `addItem` addDrink  TODO: Does not work due to type differences!
