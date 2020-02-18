module Main where

import Prelude hiding ((!!))
import Control.Monad.Random (getRandom)
import Data.Vector (Vector, replicateM, imap)
import Data.Bool (bool)
import BitsNBobs ((!!), iterateUntilM, mapTriple, pattern T, pattern F)

-- Compute the next row from the previous one
step :: Vector Bool -> Vector Bool
step row = imap (\col _ -> fill $ extract $ col) row
  where
  -- Extract the previous cells for a given position
  extract :: Int -> (Bool, Bool, Bool)
  extract col = ((row !!) . (col +)) `mapTriple` (-1, 0, 1)

  fill :: (Bool, Bool, Bool) -> Bool
  fill (T, T, T) = F
  fill (T, T, F) = T
  fill (T, F, T) = T
  fill (T, F, F) = F
  fill (F, T, T) = T
  fill (F, T, F) = T
  fill (F, F, T) = T
  fill (F, F, F) = F
  -- TODO: maybe see if this is some bit twiddling operation

-- Stop if the row is full of ones or zeros
shouldStop :: Vector Bool -> Bool
shouldStop r =
  let n = sum $ bool 0 1 <$> r
  in n == 0 || n == length r

-- Show the row in a nice compact form
render :: Vector Bool -> String
render = foldMap (show @Integer . bool 0 1)

main :: IO ()
main = do
  -- Get the number from the user
  putStrLn "Please enter a number"
  n <- readLn
  -- Generate the first row
  r0 <- replicateM n getRandom
  -- Iterate by...
  flip iterateUntilM r0 $ \r -> do
    -- ...printing the current row, and...
    putStrLn $ render r
    -- ... producing another row if appropriate
    pure $ if shouldStop r
        then Nothing
        else Just (step r)
  pure ()
