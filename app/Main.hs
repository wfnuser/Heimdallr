module Main where

import Data.Maybe (fromMaybe)
import MyLib

main :: IO ()
main = do
  -- Example using GF(7)
  let a = mkGF 7 3 -- 3 in GF(7)
      b = mkGF 7 5 -- 5 in GF(7)
  putStrLn "Working in GF(7):"
  putStrLn $ "a = " ++ show (fromMaybe (error "Invalid") a)
  putStrLn $ "b = " ++ show (fromMaybe (error "Invalid") b)

  -- Addition
  putStrLn $
    "a + b = "
      ++ show
        ( fromMaybe (error "Invalid") $ do
            x <- a
            y <- b
            add x y
        )

  -- Multiplication
  putStrLn $
    "a * b = "
      ++ show
        ( fromMaybe (error "Invalid") $ do
            x <- a
            y <- b
            mul x y
        )

  -- Inverse of a
  putStrLn $
    "a^(-1) = "
      ++ show
        ( fromMaybe (error "Invalid") $ do
            x <- a
            inv x
        )

  -- Power
  putStrLn $
    "a^3 = "
      ++ show
        ( fromMaybe (error "Invalid") $ do
            x <- a
            pow x 3
        )
