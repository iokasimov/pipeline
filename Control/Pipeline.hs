module Control.Pipeline (Pipeline, await, yield, finish, impact) where

import "base" Control.Monad (Monad ((>>=)))
import "base" Control.Applicative (Applicative (pure))
import "base" Data.Function (($), (.), flip)
import "transformers" Control.Monad.Trans.Cont (ContT (..))
import "transformers" Control.Monad.Trans.Class (lift)

newtype Producer i t r = Producer { produce :: Consumer i t r -> t r }

newtype Consumer o t r = Consumer { consume :: o -> Producer o t r -> t r }

newtype Pipe i o r t a = Pipe { pipe :: Producer i t r -> Consumer o t r -> t r }

type Pipeline i o t a r = ContT r (Pipe i o r t) a

pause :: (() -> Pipe i o r t a) -> Producer i t r -> Producer o t r
pause next ik = Producer $ \ok -> (pipe $ next ()) ik ok

suspend :: (i -> Pipe i o r t a) -> Consumer o t r -> Consumer i t r
suspend next ok = Consumer $ \v ik -> pipe (next v) ik ok

-- | Take incoming value from pipeline
await :: Pipeline i o t i r
await = ContT $ \k -> Pipe $ \ik ok -> produce ik (suspend k ok)

-- | Give a value to the future consuming
yield :: o -> Pipeline i o t () r
yield v = ContT $ \k -> Pipe $ \ik ok -> consume ok v (pause k ik)

-- | Pipeline that does nothing
finish :: Monad t => Pipeline i o t () ()
finish = ContT $ \_ -> Pipe $ \_ _ -> pure ()

-- | Do some effectful computation within pipeline
impact :: Monad t => t a -> Pipeline i o t a ()
impact e = ContT $ \next -> Pipe $ \ik ok -> e >>= \x -> pipe (next x) ik ok
