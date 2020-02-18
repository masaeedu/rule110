module Main where

import Prelude hiding ((!!))
import Control.Monad.Random (getRandom)
import Data.Vector (Vector, replicateM, imap)
import Data.Bool (bool)
import Data.Foldable (fold)
import BitsNBobs ((!!), iterateUntilM, mapTriple, pattern T, pattern F)

-- Compute the next row from the previous one
step :: Vector Bool -> Vector Bool
step vec = imap (\i _ -> next $ prev $ i) vec
  where
  -- Extract the previous cells for a given position
  prev :: Int -> (Bool, Bool, Bool)
  prev i = ((vec !!) . (i +)) `mapTriple` (-1, 0, 1)

  -- Calculate the cell below
  next :: (Bool, Bool, Bool) -> Bool
  next (T, T, T) = F
  next (T, T, F) = T
  next (T, F, T) = T
  next (T, F, F) = F
  next (F, T, T) = T
  next (F, T, F) = T
  next (F, F, T) = T
  next (F, F, F) = F
  -- TODO: maybe see if this is some bit twiddling operation

-- Stop if the row is full of ones or zeros
shouldStop :: Vector Bool -> Bool
shouldStop v =
  let n = sum $ bool 0 1 <$> v
  in n == 0 || n == length v

-- Show the row in a nice compact form
render :: Vector Bool -> String
render = fold . fmap (show @Integer . bool 0 1)

main :: IO ()
main = do
  -- Get the number from the user
  putStrLn "Please enter a number"
  n <- readLn
  -- Generate the first row
  r0 <- replicateM n getRandom
  -- Iterate by...
  flip iterateUntilM r0 $ \v -> do
    -- ...printing the current row, and...
    putStrLn $ render v
    -- ... producing another row if appropriate
    pure $ if shouldStop v
        then Nothing
        else Just (step v)
  pure ()
