
import Data.Functor.Identity

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

labels :: Tree a -> [a]
labels (Leaf l) = [l]
labels (Node l r) = labels l ++ labels r

newtype ST s m a = ST { runStateT:: s -> m (a, s)}

instance (Functor m) => Functor (ST s m) where
    fmap f m = ST (\s -> fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s)

instance (Functor m, Monad m) => Applicative (ST s m) where
    pure a = ST $ \ s -> return (a, s)
    {-# INLINE pure #-}
    ST mf <*> ST mx = ST $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance Monad m => Monad (ST s m) where
    -- return :: a -> ST s a
    return v = ST (\s -> return (v, s))
    -- (>>=) :: ST s a -> (a -> ST s b) -> ST s b
    (ST rs) >>= f = ST $ \s -> do
        (v,s') <- rs s
        runStateT (f v) s'

fresh :: Monad m => ST Int m Int
fresh = \n
    \n -> return (n, n + 1)

main :: IO ()
main = do
    let intTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

    print intTree

    print $ labels intTree
