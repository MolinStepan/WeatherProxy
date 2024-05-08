module Data.KDTree where

data TwoDTree a b
  = Node2
      { left  :: TwoDTree a b
      , val   :: (a, b)
      , right :: TwoDTree a b
      }
  | Leaf2
  deriving (Eq)

findClosest :: forall a b dist.
  (Ord a, Ord b, Ord dist) =>
  TwoDTree a b ->
  ((a,b)->(a,b)->dist) ->
  (a,b) ->
  Maybe (a,b)
findClosest root metrics (lfa,lfb) = case root of
  Leaf2       -> Nothing -- empty tree
  Node2 _ v _ -> Just $ helpera root v
  where
    helpera :: TwoDTree a b -> (a,b) -> (a,b)
    helpera node acc = case node of
      Leaf2 -> acc
      Node2 l (a,b) r -> let
        newClosest =
          if metrics acc (lfa,lfb) > metrics (a,b) (lfa,lfb)
          then (a,b)
          else acc
        nextNode = if a > lfa then l else r
        closestPrecalculated = helpera nextNode newClosest
        in undefined

    helperb :: TwoDTree a b -> (a,b) -> (a,b)
    helperb node acc = case node of
      Leaf2 -> acc
      Node2 l (a,b) r -> let
        newClosest =
          if metrics acc (lfa,lfb) > metrics (a,b) (lfa,lfb)
          then (a,b)
          else acc
        nextNode = if b > lfb then l else r
        closestPrecalculated = helpera nextNode newClosest
        in undefined

addToTree :: (Ord a, Ord b) => TwoDTree a b -> (a, b) -> TwoDTree a b
addToTree tree (va, vb) = undefined


