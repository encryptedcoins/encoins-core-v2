module Main where

import           Prelude                 hiding (Fractional (..), Num (..))

import           ZkFold.Base.Data.Vector (Vector)

import           Test                    (specEncoinsV2)

main :: IO ()
main = do
    -- Run tests here
    specEncoinsV2 @Vector

    putStrLn "\nAll tests passed!"
