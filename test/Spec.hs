{-# LANGUAGE ScopedTypeVariables #-}

import System.Plugins.Dynloader
import Test.Hspec
import Data.Dynamic
import Data.Maybe

main :: IO ()
main = hspec $ do
  describe "unasfeLoad" $ do
    it "can load foreign value"  $
      unsafeLoad [] [] "3" `shouldReturn` 3

    it "can load Prelude function"  $
      do
        plus <- unsafeLoad [] ["Prelude"] "(+)"
        return $ plus 1 2

        `shouldReturn` 3

  describe "load" $ do
    it "can return Prelude data wraped in Dynamic"  $
      do
        dyn <- load [] ["Prelude"] "Just True"
        let value = fromJust $ fromDynamic dyn
        return value

        `shouldReturn` Just True

    it "can load Prelude function wraped in Dynamic"  $
      do
        dyn <- load [] ["Prelude"] "(+):: Int -> Int -> Int"
        let (plus :: Int -> Int -> Int) = fromJust $ fromDynamic dyn
        return $ plus 1 2

        `shouldReturn` 3


