module Cost where

newtype Angle = Angle Float
newtype Cost = Cost Float deriving (Ord,Eq,Show)

instance Num Cost where
    (+) (Cost a) (Cost b) = Cost (a+b)
    (-) (Cost a) (Cost b) = Cost (a-b)
    (*) (Cost a) (Cost b) = Cost (a*b)
    negate (Cost a) = Cost (negate a)
    abs (Cost a) = Cost (abs a)
    signum (Cost a) = Cost (signum a)
    fromInteger a = Cost (fromInteger a)

instance Real Cost where
    toRational (Cost a) = toRational a

instance Fractional Cost where
    (/) (Cost a) (Cost b) = Cost (a/b)
    recip (Cost a) = Cost (recip a)
    fromRational a = Cost (fromRational a)

calcDepartCostFromAngle :: Angle -> Cost
calcDepartCostFromAngle (Angle angle) = Cost $ 1.0 * angle / pi

calcReturnCostFromAngle :: Angle -> Cost
calcReturnCostFromAngle (Angle angle) = Cost $ 4.0 * angle * angle / (pi * pi)

