{-# LANGUAGE InstanceSigs #-}

module MyStateT (MyStateT(..), myState) where

import Control.Monad.Trans.Class (MonadTrans(..))

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

myState :: Monad m => (s -> (a, s)) -> MyStateT s m a
myState f = MyStateT $ return . f

instance Functor m => Functor (MyStateT s m) where
    fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
    fmap f x = MyStateT $ \s -> fmap (\(a, sx) -> (f a, sx)) (runMyStateT x s)

instance Monad m => Applicative (MyStateT s m) where
    pure :: a -> MyStateT s m a
    pure x = MyStateT $ \s -> pure (x, s)

    (<*>) :: MyStateT s m (a -> b) -> MyStateT s m a -> MyStateT s m b
    f <*> v = MyStateT $ \s -> do
        (fx, s1) <- runMyStateT f s
        (a, s2)  <- runMyStateT v s1
        return (fx a, s2)

instance Monad m => Monad (MyStateT s m) where
    (>>=) :: MyStateT s m a -> (a -> MyStateT s m b) -> MyStateT s m b
    m >>= k = MyStateT $ \s -> do
        (a, sx) <- runMyStateT m s
        runMyStateT (k a) sx

instance MonadTrans (MyStateT s) where
    lift :: Monad m => m a -> MyStateT s m a
    lift m = MyStateT $ \s -> do
        a <- m
        return (a, s)
