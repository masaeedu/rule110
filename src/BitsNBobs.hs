module BitsNBobs where

import Data.Vector (Vector, (!))

-- Vector indexing that wraps around (and allows negative values)
(!!) :: Vector a -> Int -> a
v !! i =
  let l = length v
  in v ! ((i + l) `mod` l)

-- Given some starting value, iterates it using a @Maybe@-producing
-- monadic action until it produces @Nothing@
iterateUntilM :: Monad m => (a -> m (Maybe a)) -> a -> m a
iterateUntilM k = rec
  where
  rec a = do
    m <- k a
    case m of
      Nothing -> pure a
      Just a' -> rec a'

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a, b, c) = (f a, f b, f c)

-- Typing @True@ and @False@ is annoying
pattern T :: Bool
pattern T = True
pattern F :: Bool
pattern F = False
{-# COMPLETE T, F #-}

