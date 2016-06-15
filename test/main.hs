{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.QuickCheck.Arbitrary ()
import Control.Exception.Lifted hiding (throwTo)
import Data.IORef
import Data.Typeable
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancelWith, waitCatch)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Enclosed
import Control.Monad (forever)

{-# ANN main ("HLint: ignore Redundant do"::String) #-}
main :: IO ()
main = hspec $ do
    context "Unhandled.Exception" $ do
        -- const :: Catcher
        describe "const" $ do
            it "doesn't catch exceptions thrown from the inside" $ do
                const `catcherCatchesInside` False
            it "doesn't catch exceptions thrown from the outside" $ do
                const `catcherCatchesOutside` False
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                const `catcherCatchesDeep` False

        -- fmap Right :: Trier
        describe "fmap Right" $ do
            it "doesn't catch exceptions thrown from the inside" $ do
                fmap Right `trierCatchesInside` False
            it "doesn't catch exceptions thrown from the outside" $ do
                fmap Right `trierCatchesOutside` False
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                fmap Right `trierCatchesDeep` False

    context "Control.Exception" $ do
        describe "catch" $ do
            it "catches exceptions thrown from the inside" $ do
                catch `catcherCatchesInside` True
            it "catches exceptions thrown from the outside" $ do
                catch `catcherCatchesOutside` True
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                catch `catcherCatchesDeep` False
        describe "try" $ do
            it "catches exceptions thrown from the inside" $ do
                try `trierCatchesInside` True
            it "catches exceptions thrown from the outside" $ do
                try `trierCatchesOutside` True
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                try `trierCatchesDeep` False

    context "Control.Exception.Enclosed" $ do
        describe "catchAny" $ do
            it "catches exceptions thrown from the inside" $ do
                catchAny `catcherCatchesInside` True
            it "doesn't catch exceptions thrown from the outside" $ do
                catchAny `catcherCatchesOutside` False
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                catchAny `catcherCatchesDeep` False

        describe "catchDeep" $ do
            it "catches exceptions thrown from the inside" $ do
                catchDeep `catcherCatchesInside` True
            it "catches exceptions thrown from the outside" $ do
                catchDeep `catcherCatchesOutside` True
            it "catches exceptions lazily thrown in its pure result" $ do
                catchDeep `catcherCatchesDeep` True

        describe "tryAny" $ do
            it "catches exceptions thrown from the inside" $ do
                tryAny `trierCatchesInside` True
            it "doesn't catch exceptions thrown from the outside" $ do
                tryAny `trierCatchesOutside` False
            it "doesn't catch exceptions lazily thrown in its pure result" $ do
                tryAny `trierCatchesDeep` False

            let shouldBeShow :: Show a => a -> a -> IO ()
                shouldBeShow x y = show x `shouldBe` show y

            it "isn't fooled by BlockedIndefinitelyOnMVar" $ do
                res <- tryAny $ do
                    var <- newEmptyMVar
                    takeMVar (var :: MVar ())
                res `shouldBeShow` Left (toException BlockedIndefinitelyOnMVar)

            it "isn't fooled by BlockedIndefinitelyOnTVar" $ do
                res <- tryAny $ do
                    var <- atomically newEmptyTMVar
                    atomically $ takeTMVar (var :: TMVar ())
                res `shouldBeShow` Left (toException BlockedIndefinitelyOnSTM)

        describe "tryDeep" $ do
            it "catches exceptions thrown from the inside" $ do
                tryDeep `trierCatchesInside` True
            it "catches exceptions thrown from the outside" $ do
                tryDeep `trierCatchesOutside` True
            it "catches exceptions lazily thrown in its pure result" $ do
                tryDeep `trierCatchesDeep` True

        describe "tryAnyDeep" $ do
            it "catches exceptions thrown from the inside" $ do
                tryAnyDeep `trierCatchesInside` True
            it "doesn't catch exceptions thrown from the outside" $ do
                tryAnyDeep `trierCatchesOutside` False
            it "catches exceptions lazily thrown in its pure result" $ do
                tryAnyDeep `trierCatchesDeep` True


type Catcher = IO () -> (SomeException -> IO ()) -> IO ()
type Trier = IO () -> IO (Either SomeException ())

-- Dummy exception types used just for testing.
data DummyException = DummyException
    deriving (Show, Typeable)
instance Exception DummyException

-- A handler that fails the test if it catches the wrong type of exception.
catchAssert :: forall e. Exception e => e -> IO () -> SomeException -> IO ()
catchAssert _ act se = case fromException se of
    Just (_ :: e) -> act
    Nothing -> expectationFailure "Caught an unexpected exception"

-- Block a thread
blockIndefinitely :: IO ()
blockIndefinitely = forever $ threadDelay maxBound


-- Test whether a catcher will catch exceptions thrown from the inside.
catcherCatchesInside :: Catcher -> Bool -> IO ()
catcherCatchesInside fCatch asExpected = do
    caughtRef <- newIORef False
    thread <- async $ do
        fCatch
            (throwIO DummyException)
            (catchAssert DummyException $ writeIORef caughtRef True)
        -- No known catchers will catch an exception without also handling it.
        readIORef caughtRef `shouldReturn` True
    _ <- waitCatch thread
    readIORef caughtRef `shouldReturn` asExpected


-- Test whether a catcher will catch exceptions thrown from the outside.
catcherCatchesOutside :: Catcher -> Bool -> IO ()
catcherCatchesOutside fCatch asExpected = do
    caughtRef <- newIORef False
    baton <- newEmptyMVar
    thread <- async $ do
        fCatch
            (do putMVar baton ()
                -- DummyException can happen from here on
                blockIndefinitely)
            (catchAssert DummyException $ writeIORef caughtRef True)
        -- No known catchers will catch an exception without also handling it.
        readIORef caughtRef `shouldReturn` True
    takeMVar baton
    cancelWith thread DummyException
    _ <- waitCatch thread
    readIORef caughtRef `shouldReturn` asExpected


-- Test whether a catcher will catch exceptions lazily thrown in a pure result.
-- This is done by `return (throw DummyException)`, which will not
-- raise the exception until the return value is forced.
catcherCatchesDeep :: Catcher -> Bool -> IO ()
catcherCatchesDeep fCatch asExpected = do
    caughtRef <- newIORef False
    thread <- async $ do
        fCatch
            (return (throw DummyException))
            (catchAssert DummyException $ writeIORef caughtRef True)
    _ <- waitCatch thread
    readIORef caughtRef `shouldReturn` asExpected


-- Test whether a trier will catch exceptions thrown from the inside.
trierCatchesInside :: Trier -> Bool -> IO ()
trierCatchesInside fTry asExpected = do
    caughtRef <- newIORef False
    thread <- async $ do
        _ <- fTry (throwIO DummyException)
        writeIORef caughtRef True
    _ <- waitCatch thread
    readIORef caughtRef `shouldReturn` asExpected


-- Test whether a trier will catch exceptions thrown from the outside.
trierCatchesOutside :: Trier -> Bool -> IO ()
trierCatchesOutside fTry asExpected = do
    caughtRef <- newIORef False
    baton <- newEmptyMVar
    thread <- async $ do
        _ <- fTry $ do
            putMVar baton ()
            -- DummyException can happen from here on
            blockIndefinitely
        writeIORef caughtRef True
    takeMVar baton
    cancelWith thread DummyException
    _ <- waitCatch thread
    readIORef caughtRef `shouldReturn` asExpected


-- Test whether a trier will catch exceptions lazily thrown in a pure result.
-- This is done by `return (throw DummyException)`, which will not
-- raise the exception until the return value is forced.
trierCatchesDeep :: Trier -> Bool -> IO ()
trierCatchesDeep fTry asExpected = do
    eres <- fTry $ return $ throw DummyException
    let caughtDummyException = case eres of
            Left e
                | Just DummyException <- fromException e -> True
                | otherwise -> error "Caught an unexpected exception"
            Right _ -> False
    caughtDummyException `shouldBe` asExpected
