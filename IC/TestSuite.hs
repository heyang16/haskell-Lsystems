{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module IC.TestSuite where

import Control.Exception
import Control.Monad
import Data.List

data TestCase = forall a b . (Reformat a, Show b, DoubleAwareEquals b)
              => TestCase String (a -> b) [(a, b)]

eps :: Fractional a => a
eps = 1e-3

class Eq a => DoubleAwareEquals a where 
  doubleAwareEquals :: a -> a -> Bool
  doubleAwareEquals = (==)

instance DoubleAwareEquals Int
instance DoubleAwareEquals Bool
instance DoubleAwareEquals Char
instance DoubleAwareEquals Integer
instance DoubleAwareEquals Word

instance DoubleAwareEquals Float where
  doubleAwareEquals a b = abs (b - a) < eps

instance DoubleAwareEquals Double where
  doubleAwareEquals a b = abs (b - a) < eps

instance DoubleAwareEquals a => DoubleAwareEquals (Maybe a) where
  doubleAwareEquals Nothing Nothing   = True
  doubleAwareEquals (Just a) (Just b) = doubleAwareEquals a b
  doubleAwareEquals _ _               = False

instance (DoubleAwareEquals a, DoubleAwareEquals b) => DoubleAwareEquals (Either a b) where
  doubleAwareEquals (Left a) (Left b)   = doubleAwareEquals a b
  doubleAwareEquals (Right a) (Right b) = doubleAwareEquals a b
  doubleAwareEquals _ _                 = False

-- List comparison that does not care about order
instance {-# OVERLAPPABLE #-} DoubleAwareEquals a => DoubleAwareEquals [a] where
  doubleAwareEquals [] []       = True
  doubleAwareEquals [] _        = False
  doubleAwareEquals (x : xs) ys = case go ys of
    Nothing -> False
    Just ys -> doubleAwareEquals xs ys
    where
      go [] = Nothing
      go (y : ys)
        | x `doubleAwareEquals` y = Just ys
        | otherwise               = (y :) <$> go ys

instance (DoubleAwareEquals a, DoubleAwareEquals b) => DoubleAwareEquals (a, b) where
  doubleAwareEquals (a1, b1) (a2, b2)
    = doubleAwareEquals a1 a2 && doubleAwareEquals b1 b2

instance (DoubleAwareEquals a, DoubleAwareEquals b, DoubleAwareEquals c) => DoubleAwareEquals (a, b, c) where
  doubleAwareEquals (a1, b1, c1) (a2, b2, c2)
    = doubleAwareEquals a1 a2 && doubleAwareEquals b1 b2 && doubleAwareEquals c1 c2

goTest :: TestCase -> IO ()
goTest (TestCase name f cases) = do
  counts <- forM cases (handle majorExceptionHandler . goTestOne name f)
  let passes = filter id counts
  putStrLn $ name ++ ": " ++ show (length passes)
                  ++ " / " ++ show (length counts)
  putStrLn ""
  where
    majorExceptionHandler :: SomeException -> IO Bool
    majorExceptionHandler e = putStrLn ("Argument exception: " ++ show e) >> return False

goTestOne :: (DoubleAwareEquals x, Reformat t, Show x) => [Char] -> (t -> x) -> (t, x) -> IO Bool
goTestOne name f (input, expected) = handle exceptionHandler $ do
  r <- evaluate (f input)
  if r `doubleAwareEquals` expected
    then return True
    else failedStanza False r
  where
    failedStanza :: Show x => Bool -> x -> IO Bool
    failedStanza b x = do
      putStr . unlines $ [ " > " ++ name ++ " " ++ reformat input ++ " = " ++
                            (if b then "Exception: " else "") ++ show x
                         , "   test case expected: " ++ show expected
                         , ""
                         ]
      return False

    exceptionHandler :: SomeException -> IO Bool
    exceptionHandler = failedStanza True

class Reformat a where
  reformat :: a -> String

(==>) = (,)

mkId :: (a, b) -> (Id a, b)
mkId (x,y) = (Id x, y)

uncurry4 f (a, b, c, d) = f a b c d

uncurry5 f (a, b, c, d, e) = f a b c d e

newtype Id a = Id { unId :: a }

instance Show a => Reformat (Id a) where
  reformat = show . unId

instance (Show a, Show b, Show c, Show d, Show e) => Reformat (a,b,c,d,e) where
  reformat (a,b,c,d,e) = unwords [show a, show b, show c, show d, show e]

instance (Show a, Show b, Show c, Show d) => Reformat (a,b,c,d) where
  reformat (a,b,c,d) = unwords [show a, show b, show c, show d]

uncurry3 f (a, b, c) = f a b c

instance (Show a, Show b, Show c) => Reformat (a,b,c) where
  reformat (a,b,c) = unwords [show a, show b, show c]

instance Reformat Char where
  reformat c = show c

instance (Show a, Show b) => Reformat (a,b) where
  reformat (a,b) = unwords [show a, show b]

instance Reformat Int where
  reformat x = show x

instance Show a => Reformat [a] where
  reformat = show

instance Show ([a] -> Int) => Reformat ([a] -> Int) where
  reformat = show

instance Show (Int -> Int) => Reformat (Int -> Int) where
  reformat = show