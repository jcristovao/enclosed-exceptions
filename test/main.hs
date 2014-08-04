{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.QuickCheck.Arbitrary ()
import Control.Exception.Lifted hiding (throwTo)
import Data.IORef
import Data.Typeable
import Control.Monad.IO.Class
import Control.Concurrent (throwTo, threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Exception.Enclosed

{-# ANN main ("HLint: ignore Redundant do"::String) #-}
main :: IO ()
main = hspec $ do
    describe "any exceptions" $ do
        it "catchAny" $ do
            failed <- newIORef 0
            tid <- forkIO $ do
                catchAny
                    (threadDelay 20000)
                    (const $ writeIORef failed 1)
                writeIORef failed 2
            threadDelay 10000
            throwTo tid DummyException
            threadDelay 50000
            didFail <- readIORef failed
            liftIO $ didFail `shouldBe` (0 :: Int)
        it "tryAny" $ do
            failed <- newIORef False
            tid <- forkIO $ do
                _ <- tryAny $ threadDelay 20000
                writeIORef failed True
            threadDelay 10000
            throwTo tid DummyException
            threadDelay 50000
            didFail <- readIORef failed
            liftIO $ didFail `shouldBe` False
        it "tryAnyDeep" $ do
            eres <- tryAnyDeep $ return $ throw DummyException
            case eres of
                Left e
                    | Just DummyException <- fromException e -> return ()
                    | otherwise -> error "Expected a DummyException"
                Right () -> error "Expected an exception" :: IO ()

    it "MVar deadlocks simonmar/async#14" $ do
        res <- tryAny $ newEmptyMVar >>= takeMVar
        case res of
            Left _ -> return ()
            Right () -> error "Expected Left value"

data DummyException = DummyException
    deriving (Show, Typeable)
instance Exception DummyException


