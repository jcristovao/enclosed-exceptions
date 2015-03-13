{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.QuickCheck.Arbitrary ()
import Control.Exception.Lifted hiding (throwTo)
import Data.IORef
import Data.Typeable
import Control.Concurrent.Async (async, cancelWith, waitCatch)
import Control.Concurrent.MVar
import Control.Exception.Enclosed

{-# ANN main ("HLint: ignore Redundant do"::String) #-}
main :: IO ()
main = hspec $ do
    context "Unhandled.Exception" $ do
        -- const :: Catcher
        describe "const" $ do
            it "doesn't catch inside exceptions" $ do
                const `catcherCatchesInside` False
            it "doesn't catch outside exceptions" $ do
                const `catcherCatchesOutside` False
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                const `catcherCatchesDeep` False

        -- fmap Right :: Trier
        describe "fmap Right" $ do
            it "doesn't catch inside exceptions" $ do
                fmap Right `trierCatchesInside` False
            it "doesn't catch outside exceptions" $ do
                fmap Right `trierCatchesOutside` False
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                fmap Right `trierCatchesDeep` False

    context "Control.Exception" $ do
        describe "catch" $ do
            it "catches inside exceptions" $ do
                catch `catcherCatchesInside` True
            it "catches outside exceptions" $ do
                catch `catcherCatchesOutside` True
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                catch `catcherCatchesDeep` False
        describe "try" $ do
            it "catches inside exceptions" $ do
                try `trierCatchesInside` True
            it "catches outside exceptions" $ do
                try `trierCatchesOutside` True
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                try `trierCatchesDeep` False

    context "Control.Exception.Enclosed" $ do
        describe "catchAny" $ do
            it "catches inside exceptions" $ do
                catchAny `catcherCatchesInside` True
            it "doesn't catch outside exceptions" $ do
                catchAny `catcherCatchesOutside` False
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                catchAny `catcherCatchesDeep` False

        describe "catchDeep" $ do
            it "catches inside exceptions" $ do
                catchDeep `catcherCatchesInside` True
            it "catches outside exceptions" $ do
                catchDeep `catcherCatchesOutside` True
            it "catches exceptions lazily raised in its pure result" $ do
                catchDeep `catcherCatchesDeep` True

        describe "tryAny" $ do
            it "catches inside exceptions" $ do
                tryAny `trierCatchesInside` True
            it "doesn't catch outside exceptions" $ do
                tryAny `trierCatchesOutside` False
            it "doesn't catch exceptions lazily raised in its pure result" $ do
                tryAny `trierCatchesDeep` False

        describe "tryDeep" $ do
            it "catches inside exceptions" $ do
                tryDeep `trierCatchesInside` True
            it "catches outside exceptions" $ do
                tryDeep `trierCatchesOutside` True
            it "catches exceptions lazily raised in its pure result" $ do
                tryDeep `trierCatchesDeep` True

        describe "tryAnyDeep" $ do
            it "catches inside exceptions" $ do
                tryAnyDeep `trierCatchesInside` True
            it "doesn't catch outside exceptions" $ do
                tryAnyDeep `trierCatchesOutside` False
            it "catches exceptions lazily raised in its pure result" $ do
                tryAnyDeep `trierCatchesDeep` True


type Catcher = IO () -> (SomeException -> IO ()) -> IO ()
type Trier = IO () -> IO (Either SomeException ())


-- A handler that fails the test if it catches the wrong type of exception.
catchAssert :: forall e. Exception e => e -> IO () -> SomeException -> IO ()
catchAssert _ act se = case fromException se of
    Just (_ :: e) -> act
    Nothing -> expectationFailure "Caught an unexpected exception"

catcherCatchesInside :: Catcher -> Bool -> IO ()
catcherCatchesInside fCatch asExpected = do
    caughtRef <- newIORef False
    thread <- async $ do
        fCatch
            (throw DummyExceptionInternal)
            (catchAssert DummyExceptionInternal $ writeIORef caughtRef True)
        caught <- readIORef caughtRef
        caught `shouldBe` True
    _ <- waitCatch thread
    caught <- readIORef caughtRef
    caught `shouldBe` asExpected


catcherCatchesOutside :: Catcher -> Bool -> IO ()
catcherCatchesOutside fCatch asExpected = do
    caughtRef <- newIORef False
    s :: Swapper Int <- newSwapper
    thread <- async $ do
        fCatch
            (swapOut s 1)
            (catchAssert DummyException $ writeIORef caughtRef True)
        caught <- readIORef caughtRef
        caught `shouldBe` True
    swapDef s 1 $ cancelWith thread DummyException
    _ <- waitCatch thread
    caught <- readIORef caughtRef
    caught `shouldBe` asExpected


catcherCatchesDeep :: Catcher -> Bool -> IO ()
catcherCatchesDeep fCatch asExpected = do
    caughtRef <- newIORef False
    s :: Swapper Int <- newSwapper
    thread <- async $ do
        fCatch
            (swapOut s 1 >> return (throw DummyExceptionInternal))
            (catchAssert DummyExceptionInternal $ writeIORef caughtRef True)
        swapOut s 2
        expectationFailure "Thread didn't die when it should have"
    swapDef s 1 $ return ()
    swapDef s 2 $ cancelWith thread DummyException
    _ <- waitCatch thread
    caught <- readIORef caughtRef
    caught `shouldBe` asExpected


trierCatchesInside :: Trier -> Bool -> IO ()
trierCatchesInside fTry asExpected = do
    caughtRef <- newIORef False
    thread <- async $ do
        _ <- fTry (throw DummyException)
        writeIORef caughtRef True
    _ <- waitCatch thread
    caught <- readIORef caughtRef
    caught `shouldBe` asExpected


trierCatchesOutside :: Trier -> Bool -> IO ()
trierCatchesOutside fTry asExpected = do
    caughtRef <- newIORef False
    s :: Swapper Int <- newSwapper
    thread <- async $ do
        _ <- fTry $ swapOut s 1
        writeIORef caughtRef True
    swapDef s 1 $ cancelWith thread DummyException
    _ <- waitCatch thread
    caught <- readIORef caughtRef
    caught `shouldBe` asExpected


trierCatchesDeep :: Trier -> Bool -> IO ()
trierCatchesDeep fTry asExpected = do
    eres <- fTry $ return $ throw DummyException
    let caughtDummyException = case eres of
            Left e
                | Just DummyException <- fromException e -> True
                | otherwise -> error "Caught an unexpected exception"
            Right _ -> False
    caughtDummyException `shouldBe` asExpected


-- Dummy exception types used just for testing.
data DummyException = DummyException
    deriving (Show, Typeable)
instance Exception DummyException

data DummyExceptionInternal = DummyExceptionInternal
    deriving (Show, Typeable)
instance Exception DummyExceptionInternal


-- Swapper is a utility for synchronizing concurrent threads.
-- One thread should use exclusively swapOut,
-- while the other thread uses exclusively swapDef.
data Swapper label = Swapper
    { _mBeginRoutine :: MVar label
    , _mEndRoutine :: MVar label
    }

swapOut :: Eq label => Swapper label -> label -> IO ()
swapOut (Swapper mBeginRoutine mEndRoutine) label = do
    putMVar mBeginRoutine label
    takeMVar mEndRoutine >>= swapCheck label

swapDef :: Eq label => Swapper label -> label -> IO a -> IO a
swapDef (Swapper mBeginRoutine mEndRoutine) label body = do
    takeMVar mBeginRoutine >>= swapCheck label
    a <- body
    putMVar mEndRoutine label
    return a

data SwapException = SwapException
  deriving (Show, Typeable)
instance Exception SwapException

swapCheck :: Eq label => label -> label -> IO ()
swapCheck l1 l2 | l1 == l2 = return ()
swapCheck _ _ = throwIO SwapException

newSwapper :: Eq label => IO (Swapper label)
newSwapper = do
    mBeginRoutine <- newEmptyMVar
    mEndRoutine  <- newEmptyMVar
    return (Swapper mBeginRoutine mEndRoutine)
