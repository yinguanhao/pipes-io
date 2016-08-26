{-# Language LambdaCase, Rank2Types #-}
module Pipes.IOStream (
    -- * IStream and OStream
    IStream,
    OStream,
    -- * Convert between IOStreams and pipes
    mkIStream,
    mkOStream,
    fromIStream,
    fromOStream,
    -- * Reading
    readOne,
    readWith,
    -- * Wrapping
    wrapIStream,
    -- * Writting
    write,
    writeProducer,
    -- * Detecting end of stream
    isEnded,
    -- * Connect and resume for consumer
    (>->+),

    -- * Re-exports
    Producer,
    Consumer,
    Parser
) where

import Control.Monad
import Data.IORef
import Pipes
import Pipes.Internal
import Pipes.Parse

newtype IStream a = IS (IORef (Producer a IO ()))

newtype OStream a = OS (IORef (Consumer a IO ()))

mkIStream :: Producer a IO () -> IO (IStream a)
mkIStream p = liftM IS $ newIORef p

mkOStream :: Consumer a IO () -> IO (OStream a)
mkOStream c = liftM OS $ newIORef c

fromIStream :: IStream a -> IO (Producer a IO ())
fromIStream (IS rp) = readIORef rp

fromOStream :: OStream a -> IO (Consumer a IO ())
fromOStream (OS rc) = readIORef rc

wrapIStream :: IStream a -> (Producer a IO () -> Producer b IO ()) ->
               IO (IStream b)
wrapIStream i f = fromIStream i >>= mkIStream . f

readOne :: IStream a -> IO (Maybe a)
readOne (IS rp) = do
    p <- readIORef rp
    next p >>= \case
        Left () -> return Nothing
        Right (a, p') -> writeIORef rp p' >> return (Just a)

readWith :: IStream a -> Parser a IO r -> IO r
readWith (IS rp) parser = do
    p <- readIORef rp
    (r, p') <- runStateT parser p
    writeIORef rp p'
    return r

write :: OStream a -> a -> IO ()
write o a = writeProducer o (yield a)

writeProducer :: OStream a -> Producer a IO () -> IO ()
writeProducer (OS rc) p = readIORef rc >>= (p >->+) >>= writeIORef rc

isEnded :: OStream a -> IO Bool
isEnded (OS rc) = readIORef rc >>= \case
    Pure _ -> return True
    _ -> return False

{- | Pairs each await in the consumer with an yield in the producer. Returns
remaining consumer. -}
(>->+) :: Monad m => Producer a m x -> Consumer a m y -> m (Consumer a m y)
p0 >->+ c0 = go p0 c0
 where
  go p c = case c of
    Request _ f -> go1 p
     where
      go1 = \case
        Respond a g -> g () `go` f a
        M p' -> go1 =<< p'
        Pure _ -> return c
        Request v _ -> closed v
    M c' -> go p =<< c'
    Pure _ -> return c
    Respond v _ -> closed v
