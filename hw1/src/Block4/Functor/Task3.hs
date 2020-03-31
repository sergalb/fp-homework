{-# LANGUAGE InstanceSigs #-}
module Block4.Functor.Task3 where


data NonEmpty a = a :| [a]

instance Functor NonEmpty where
  fmap ::  (a -> b) -> NonEmpty a -> NonEmpty b
  fmap fun (x :| xs) = let list = fmap fun (x:xs)
                           in head list :| tail list


instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap fun (x :| xs) = foldMap fun (x:xs)


instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (fun :| funs) <*> (x :| xs) = let list = (fun:funs) <*> (x:xs)
                             in head list :| tail list
                                                          
