
-- import Data.Functor.Identity

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

labels :: Tree a -> [a]
labels (Leaf l) = [l]
labels (Node l r) = labels l ++ labels r

instance Functor Tree where
    fmap f (Leaf l) = Leaf $ f l
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

newtype St s a = St { runState:: s -> (a, s)}

-- NOTE: Not needed
-- state :: (s -> (a, s)) -> St s a
-- state = St

instance Functor (St s) where
    fmap f (St sfn) = St (\s -> let (a, s') = sfn s in (f a, s'))

instance Applicative (St s) where
    pure a = St $ \s -> (a,s)
    St sf <*> St sx = St $ \s ->
        let (f, s') = sf s
            (x, s'') = sx s'
        in (f x, s'')

instance Monad (St s) where
    return = pure
    (St sfn) >>= f = St $ \s -> case sfn s of
        (a, s') -> runState (f a) s'

fresh :: St Int Int
fresh = St (\n -> (n, n + 1))

freshDown :: St Int Int
freshDown = St (\n -> (n, n - 1))

label :: Tree a -> St Int (Tree Int)
label (Leaf x) = do n <- fresh
                    return (Leaf n)
label (Node l r) = do l' <- label l
                      r' <- label r
                      return (Node l' r')

labelWith :: Tree a -> St Int Int -> St Int (Tree Int)
labelWith (Leaf x) lf = do n <- lf
                           return (Leaf n)
labelWith (Node l r) lf = do l' <- labelWith l lf
                             r' <- labelWith r lf
                             return (Node l' r')

main :: IO ()
main = do
    -- Just testing out St (my State)
    let st = St (\s -> (s,s+1))
    let newState = runState st 0
    putStrLn $ "Initial state: " ++ show newState
    let nextState = runState st $ snd newState
    putStrLn $ "Next state: " ++ show nextState

    -- Create a float tree
    let floatTree = Node (Leaf 1.0) (Node (Leaf 2.0) (Leaf 3.0))

    -- Show it and via labels
    putStrLn $ "Float tree: " ++ show floatTree
    putStrLn $ "    labels: " ++ show (labels floatTree)

    -- Use fmap to double it
    let doubledTree = fmap (*2) floatTree
    putStrLn $ "Doubled   : " ++ show doubledTree

    putStrLn "\nLabelling Tree"
    let newTreeState = runState (label floatTree) 100

    print $ fst newTreeState
    print $ snd newTreeState

    putStrLn "\nLabelling Tree with 'freshDown'"
    let newTreeState = runState (labelWith floatTree freshDown) 100

    print $ fst newTreeState
    print $ snd newTreeState
