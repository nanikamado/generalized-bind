module Main

import Control.Monad.State


%hide (>>=)


interface GJoin t a where
    gjoin : t a -> a

Monad m => GJoin m (m a) where
    gjoin = join

Applicative m => Traversable t => GJoin t a => GJoin t (m a) where
    gjoin a = gjoin <$> sequence a

interface MultiStackBind a b c where
    (>>=) : a -> (b -> c) -> c

MultiStackBind a a b where
    m >>= k = k m

GJoin f c => Functor f => MultiStackBind a b c => MultiStackBind (f a) b c where
    m >>= k = gjoin $ (>>= k) <$> m

infixl 1 ...
(...) : a -> (a -> b) -> b
(...) = flip ($)

example1 : Maybe (Either String Integer)
example1 = do
    a <- (Just $ Right 1)
            ... the (Maybe (Either String Integer))
    b <- Right 2
            ... the (Either String Integer)
    c <- Just 3
    pure . pure $ a + b + c

example2 : Maybe (Either String Integer)
example2 = do
    a <- (Just $ Right 1)
            ... the (Maybe (Either String Integer))
    b <- Left "error"
            ... the (Either String Integer)
    c <- Just 3
    pure . pure $ a + b + c

example3 : List (Either String (Maybe Integer))
example3 = do
    a <- Just 1
    b <- Right 2
            ... the (Either String Integer)
    c <- [0, 1, 2, 3]
    d <- Just 1
    pure . pure . pure $ a + b + c + d

example4 : IO (List (Either String (Maybe Integer)))
example4 = do
    a <- Just 1
    printLn "hoge"
    b <- Right 2
            ... the (Either String Integer)
    c <- [0, 1, 2, 3]
    d <- Just 1
    pure . pure . pure . pure $ a + b + c + d


-- example5 : IO (State Integer ())
-- example5 = do
--     u <- put 1 ... the (State Integer ())
--     pure $ the () u
--     pure . pure $ ()

-- Error: While processing right hand side of example5. Can't find an implementation
-- for MultiStackBind (StateT Integer Identity ()) () (IO (State Integer ())).



main : IO ()
main = do
    printLn example1
    printLn example2
    printLn example3
    printLn $ !example4 ... the (List (Either String (Maybe Integer)))


