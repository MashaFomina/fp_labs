data PeanoNumber = Zero | Succ (PeanoNumber) | Pred (PeanoNumber) deriving Show

isSimple :: PeanoNumber -> Bool
isSimple Zero = True
isSimple (Succ (Pred _)) = False
isSimple (Pred (Succ _)) = True
isSimple (Succ num) = isSimple num
isSimple (Pred num) = isSimple num

simplify' :: PeanoNumber -> PeanoNumber
simplify' (Zero) = Zero
simplify' (Succ (Pred num)) = simplify' num
simplify' (Pred (Succ num)) = simplify' num
simplify' (Succ num) = Succ $ simplify' num
simplify' (Pred num) = Pred $ simplify' num

simplify :: PeanoNumber -> PeanoNumber
simplify num = let simplified = simplify' num in
                if (isSimple simplified) then simplified
                else simplify simplified

simpleEQ :: PeanoNumber -> PeanoNumber -> Bool
simpleEQ Zero Zero = True
simpleEQ Zero _ = False
simpleEQ _ Zero = False
simpleEQ (Succ l) (Succ r) = l `simpleEQ` r
simpleEQ (Succ l) (Pred r) = False
simpleEQ (Pred l) (Succ r) = False
simpleEQ (Pred l) (Pred r) = l `simpleEQ` r

simpleLEQ :: PeanoNumber -> PeanoNumber -> Bool
simpleLEQ Zero Zero = True
simpleLEQ Zero (Succ _) = True
simpleLEQ Zero (Pred _) = False
simpleLEQ (Succ _) Zero = False
simpleLEQ (Pred _) Zero = True
simpleLEQ (Succ l) (Succ r) = l `simpleLEQ` r
simpleLEQ (Succ l) (Pred r) = False
simpleLEQ (Pred l) (Succ r) = True
simpleLEQ (Pred l) (Pred r) = l `simpleLEQ` r

simpleDIV :: PeanoNumber -> PeanoNumber -> PeanoNumber
simpleDIV l r = let dif = l - r in
                    if (dif >= Zero) then
                      (simpleDIV dif r) + 1
                    else 0

-- class types
instance Eq PeanoNumber where
  (==) l r = simpleEQ (simplify l) (simplify r)

instance Ord PeanoNumber where
  (<=) l r = simpleLEQ (simplify l) (simplify r)

instance Num PeanoNumber where
  (+) Zero r = r
  (+) l Zero = l
  (+) (Succ l) r = Succ (l + r)
  (+) (Pred l) r = Pred (l + r)

  negate Zero = Zero
  negate (Succ num) = Pred (negate num)
  negate (Pred num) = Succ (negate num) 

  fromInteger x | x == 0 = Zero
                | x < 0 = Pred (fromInteger (x + 1))
                | otherwise = Succ (fromInteger (x - 1))

  signum Zero = Zero
  signum (Succ (Pred num)) = signum num
  signum (Pred (Succ num)) = signum num
  signum (Succ num) = Succ Zero
  signum (Pred num) = Pred Zero

  abs num = if (signum num < Zero) then negate num else num

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) (Succ l) r = (r+(l*r))
  (*) (Pred l) r = if (signum l == signum r) then  (r + (l * r))
                       else if (signum l < Zero) then negate(r + ((negate l) * r))
                       else let nr = negate r in negate (nr + (l * nr))

instance Enum PeanoNumber where
  toEnum num | num == 0 = Zero
             | num < 0 = Pred (toEnum $ num + 1)
             | otherwise = Succ (toEnum $ num - 1) 
  fromEnum Zero = 0
  fromEnum (Succ l) = (fromEnum l) + 1
  fromEnum (Pred l) = (fromEnum l) - 1

instance Real PeanoNumber where
   toRational num = toRational (toInteger num)

instance Integral PeanoNumber where
  quotRem l r = let isNeg = (signum l) == (signum r) in
                    let div = simpleDIV (abs l) (abs r) in
                    if (isNeg) then (div, simplify $ l - div * r) else (negate div, simplify $ l - div * r)

  toInteger Zero = 0
  toInteger (Succ l) = (toInteger l) + 1
  toInteger (Pred l) = (toInteger l) - 1