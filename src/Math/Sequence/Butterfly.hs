module Math.Sequence.Butterfly where

import Data.Tree

-- |Separate a list into two by taking alternate elements.  The first goes into
-- the left, the next goes into the right, etc.
alternates :: [a] -> ([a], [a])
alternates (x:y:zs) = (x:xs, y:ys)
    where
        ~(xs,ys) = alternates zs
alternates xs = (xs,[])

butterfly :: [a] -> Forest a
butterfly [] = []
butterfly xs = 
    [ Node y (butterfly ys)
    | y:ys <- uncurry (\a b -> [a,b]) (alternates xs)
    ]
