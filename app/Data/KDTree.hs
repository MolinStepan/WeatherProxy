module Data.KDTree where

-- todo

data TwoDTree a
  = Node2
      { left  :: TwoDTree a
      , val   :: (a, a)
      , right :: TwoDTree a
      }
  | Leaf2
  deriving (Eq)

findClosest :: forall a.
  (Num a, Ord a) =>
  TwoDTree a ->
  (a,a) ->
  Maybe (a,a)
findClosest root (x,y) = case root of
  Leaf2       -> Nothing -- empty tree
  Node2 _ v _ -> Just $ helperX root v
  where
    helperX :: TwoDTree a -> (a,a) -> (a,a)
    helperX node (ax,ay) = case node of
      Leaf2 -> (ax,ay)
      Node2 l (x',y') r -> let
        newClosest =
          if (ax - x)^2 + (ay - y)^2 > (x' - x)^2 + (y' - y)^2
          then (x',y')
          else (ax,ay)

        (nextNode,otherNode) = if ax > x then (r,l) else (l,r)

        (cx,cy) = helperY nextNode newClosest

        closest = if (ax - x)^2 > (cx - x)^2 + (cy - y)^2
          then (cx,cy)
          else branchX otherNode (cx,cy)

        in undefined

    branchX :: TwoDTree a -> (a,a) -> (a,a)
    branchX node closest = case node of
      Leaf2             -> closest
      Node2 l (x',y') r -> undefined


    branchY :: TwoDTree a -> (a,a) -> (a,a)
    branchY = undefined

    helperY :: TwoDTree a -> (a,a) -> (a,a)
    helperY node (ax,ay) = undefined

addToTree :: forall a b.
  (Ord a, Num a) =>
  TwoDTree a ->
  (a,a) ->
  TwoDTree a
addToTree tree (va, vb) = undefined


