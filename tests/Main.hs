module Main(main) where

import Test.Hspec

import Data.IORef

import Pipes
import Pipes.IOStream

main :: IO ()
main = hspec $ do
  describe ">->+" $ do
    it "should return remaining consumer" $ do
      r <- newIORef []
      let c = consumerPrepend r

      c1 <- yield 0 >->+ c
      r `shouldRef` [(0, 0)]

      c2 <- (yield 3 >> yield 7) >->+ c1
      r `shouldRef` [(2, 7), (1, 3), (0, 0)]

      runEffect $ (yield 23 >> yield 37) >-> c2
      r `shouldRef` [(4, 37), (3, 23), (2, 7), (1, 3), (0, 0)]

  describe "writeProducer" $ do
    it "should work" $ do
      r <- newIORef []
      let c = consumerPrepend r
      o <- mkOStream c

      writeProducer o $ yield 3 >> yield 7
      r `shouldRef` [(1, 7), (0, 3)]

      writeProducer o $ yield 11 >> yield 13
      r `shouldRef` [(3, 13), (2, 11), (1, 7), (0, 3)]

consumerPrepend :: IORef [(Int, Int)] -> Consumer Int IO ()
consumerPrepend ref = go 0
  where
    go n = do
      x <- await
      lift $ modifyIORef' ref ((n, x):)
      go (n + 1)

shouldRef :: (Show a, Eq a) => IORef a -> a -> Expectation
shouldRef r x = readIORef r `shouldReturn` x
