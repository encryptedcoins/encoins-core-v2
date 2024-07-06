module Main where

import           Prelude                                     hiding (Fractional (..), Num (..))

main :: IO ()
main = do
    -- Run tests here
    specEncoinsV2

    putStrLn "\nAll tests passed!"
