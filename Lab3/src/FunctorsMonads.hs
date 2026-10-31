module FunctorsMonads where

class Functor' f where
  (<$$>) :: (a -> b) -> f a -> f b

infixl 4 <$$>

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

infixl 4 <**>

class Applicative' m => Monad' m where
  (>>==) :: m a -> (a -> m b) -> m b
  (>>==) x f = join' (f <$$> x)
  join' :: m (m a) -> m a
  join' x = x >>== id

instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)

instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing

instance Monad' Maybe where
  Nothing >>== _ = Nothing
  Just x >>== f = f x
  join' Nothing = Nothing
  join' (Just x) = x

instance Functor' [] where
  f <$$> xs = map f xs

instance Applicative' [] where
  pure' x = [x]
  fs <**> xs = [f x | f <- fs, x <- xs]

instance Monad' [] where
  xs >>== f = [y | x <- xs, y <- f x]
  join' xss = [x | xs <- xss, x <- xs]

liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f x y = f <$$> x <**> y

seqA :: Applicative' f => [f a] -> f [a]
seqA [] = pure' []
seqA (x:xs) = liftA2' (:) x (seqA xs)

traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA _ [] = pure' []
traverseA f (x:xs) = liftA2' (:) (f x) (traverseA f xs)

filterA :: Applicative' f => (a -> f Bool) -> [a] -> f [a]
filterA _ [] = pure' []
filterA f (x:xs) = liftA2' (\b rest -> if b then x:rest else rest) (f x) (filterA f xs)

composeM :: Monad' m => (b -> m c) -> (a -> m b) -> (a -> m c)
composeM f g x = g x >>== f

instance Functor' (Either t) where
  f <$$> Right x = Right (f x)
  _ <$$> Left e = Left e

instance Applicative' (Either t) where
  pure' = Right
  Right f <**> Right x = Right (f x)
  Left e <**> _ = Left e
  _ <**> Left e = Left e

instance Monad' (Either t) where
  Right x >>== f = f x
  Left e >>== _ = Left e
  join' (Right x) = x
  join' (Left e) = Left e

instance Functor' ((->) t) where
  f <$$> g = f . g

instance Applicative' ((->) t) where
  pure' x = \_ -> x
  f <**> g = \x -> f x (g x)

instance Monad' ((->) t) where
  f >>== g = \x -> g (f x) x
  join' f = \x -> f x x