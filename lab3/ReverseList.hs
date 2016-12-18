data ReverseList a = RNil | RCons (ReverseList a) a

instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) RNil _ = False
  (==) _ RNil = False
  (==) (RCons at ah) (RCons bt bh) = ah == bh && at == bt

instance (Ord a) => Ord (ReverseList a) where
  (<=) RNil RNil = True
  (<=) RNil _ = True
  (<=) _ RNil = False
  (<=) (RCons at ah) (RCons bt bh) = if (ah <= bh) then at <= bt
                                      else False

instance (Show a) => Show (ReverseList a) where
  show RNil = "[]"
  show list = "[" ++ (showList1 list) ++ "]"

showList1 :: (Show a) => ReverseList a -> String
showList1 RNil = ""
showList1 (RCons RNil h) = show h
showList1 (RCons at ah) = (showList1 at) ++ ", " ++ (show ah)

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend RNil list = list
  mappend list RNil = list
  mappend list (RCons at ah) = RCons (mappend list at) ah

instance Functor ReverseList where
  fmap f RNil = RNil
  fmap f (RCons at ah) = RCons (fmap f at) (f ah)

toList :: ReverseList a -> [a]
toList RNil = []
toList (RCons t h) = h:(toList t)

fromList :: [a] -> ReverseList a
fromList [] = RNil
fromList (h:t) = RCons (fromList t) h