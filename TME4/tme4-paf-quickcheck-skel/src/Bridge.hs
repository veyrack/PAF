module Bridge where

data IslandBridge =
  BridgeOpened Int Int Int Int 
  | BridgeClosed Int Int Int
  deriving Show

-- smart constructor
mkBridge :: Int -> Int -> Int -> Int -> IslandBridge
mkBridge lim nbTo nbI nbFrom
  | nbTo + nbI + nbFrom < lim = BridgeOpened lim nbTo nbI nbFrom
  | nbTo + nbI + nbFrom == lim = BridgeClosed lim nbTo nbFrom
  | otherwise = error "Wrong bridge (limit exceeded)"

-- accessors
nbCarsOnIsland :: IslandBridge -> Int -- Voitures sur l'ile
nbCarsOnIsland (BridgeOpened _ _ nbI _) = nbI
nbCarsOnIsland (BridgeClosed lim nbTo nbFrom) = lim - (nbFrom + nbTo)

nbCarsToIsland :: IslandBridge -> Int -- Voitures qui vont vers l'ile 
nbCarsToIsland (BridgeOpened _ nbTo _ _) = nbTo
nbCarsToIsland (BridgeClosed _ nbTo _) = nbTo

nbCarsFromIsland :: IslandBridge -> Int -- Voitures qui viennent de l'ile
nbCarsFromIsland (BridgeOpened _ _ _ nbFrom) = nbFrom
nbCarsFromIsland (BridgeClosed _ _ nbFrom) = nbFrom

nbCars :: IslandBridge -> Int -- Nombres de voitures total
nbCars (BridgeOpened _ nbTo nbI nbFrom) = nbTo + nbI + nbFrom
nbCars (BridgeClosed lim _ _) = lim

bridgeLimit :: IslandBridge -> Int -- capacitie max du pont
bridgeLimit (BridgeOpened lim _ _ _) = lim
bridgeLimit (BridgeClosed lim _ _) = lim

-- invariant
islandBridge_inv :: IslandBridge -> Bool
islandBridge_inv b@(BridgeOpened lim _ _ _) =
  0 <= nbCars b && nbCars b <= lim
islandBridge_inv b@(BridgeClosed lim _ _) =
  nbCars b == lim

-- initialisation
initBridge :: Int -> IslandBridge
initBridge lim = mkBridge lim 0 0 0

initBridge_pre :: Int -> Bool
initBridge_pre lim = lim > 0

-- opération : entrée du continent vers l'île
enterToIsland :: IslandBridge -> IslandBridge
enterToIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim (nbTo+1) nbI nbFrom
enterToIsland (BridgeClosed _ _ _) = error "Cannot enter bridge to island"

enterToIsland_pre :: IslandBridge -> Bool
enterToIsland_pre b@(BridgeOpened lim _ _ _) = nbCars b < lim
enterToIsland_pre (BridgeClosed _ _ _) = False

leaveToIsland :: IslandBridge -> IslandBridge
leaveToIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim (nbTo-1) (nbI+1) nbFrom
leaveToIsland (BridgeClosed _ _ _) = error "Cannot enter island"

leaveToIsland_pre :: IslandBridge -> Bool
leaveToIsland_pre b@(BridgeOpened lim _ _ _) = bridgeLimit b < lim && nbCarsToIsland b > 0 && nbCars b < lim && nbCars b > 0
leaveToIsland_pre (BridgeClosed _ _ _) = False

enterFromIsland :: IslandBridge -> IslandBridge
enterFromIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim nbTo (nbI-1) (nbFrom+1)
enterFromIsland (BridgeClosed _ _ _) = error "Cannot leave island"

enterFromIsland_pre :: IslandBridge -> Bool
enterFromIsland_pre b@(BridgeOpened lim _ _ _) = nbCarsOnIsland b > 0 && bridgeLimit b < lim
enterFromIsland_pre (BridgeClosed _ _ _) = False

leaveFromIsland :: IslandBridge -> IslandBridge
leaveFromIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim nbTo nbI (nbFrom-1)
leaveFromIsland (BridgeClosed _ _ _) = error "Cannot leave bridge"

leaveFromIsland_pre :: IslandBridge -> Bool
leaveFromIsland_pre b@(BridgeOpened lim _ _ _) = nbCarsFromIsland b > 0 && bridgeLimit b < lim
leaveFromIsland_pre (BridgeClosed _ _ _) = False
