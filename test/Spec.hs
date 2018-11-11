{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

import System.Plugins.Dynloader
import Test.Hspec
import Data.Dynamic
import Data.Maybe

main :: IO ()
main = hspec $ do
  it "can load foreign value"  $ do
      unsafeLoad [] [] "3" `shouldReturn` 3

  it "can load Prelude function"  $ do
      do
        plus <- unsafeLoad [] ["Prelude"] "(+)"
        return $ plus 1 2

        `shouldReturn` 3
