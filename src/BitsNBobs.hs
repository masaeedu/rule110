module BitsNBobs where

import Data.Vector (Vector, (!))

(!!) :: Vector a -> Int -> a
v !! i = let l = length v
  in v ! ((i + l) `mod` l)

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

-- Typing True and False is annoying
pattern T :: Bool
pattern T = True
pattern F :: Bool
pattern F = False
{-# COMPLETE T, F #-}

