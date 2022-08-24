{-# LANGUAGE ExistentialQuantification #-}

module Coreutils.Util where

class Util a where
  run :: a -> [String] -> IO ()

data Utility = forall a. Util a => Utility a
instance Util Utility where
  run (Utility a) = run a
