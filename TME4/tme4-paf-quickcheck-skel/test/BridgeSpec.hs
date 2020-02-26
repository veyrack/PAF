module BridgeSpec where

import Test.Hspec
import Test.QuickCheck

import Bridge

prop_initBridge_inv :: Int -> Property
prop_initBridge_inv lim =
  (initBridge_pre lim) ==> property $ islandBridge_inv (initBridge lim)
-- quickCheck prop_initBridge_inv

bridgeSpecInit = do
  describe "initBridge" $ do
    it "preserves the invariant" $ property $ \lim -> prop_initBridge_inv lim

-- Exemple de générateur aléatoire monadique (cf. cours 8)
genBridgeFree :: Gen IslandBridge
genBridgeFree = do
    lim <- choose (1, 100)  -- la limite initiale
    nbI <- choose (0, 50)
    nbTo <- choose (0, 50)
    nbFrom <- choose (0, 50)
    return $ if nbI + nbTo + nbFrom == lim
             then BridgeClosed lim nbTo nbFrom
             else if nbI + nbTo + nbFrom >= lim
               then BridgeClosed lim nbTo nbFrom
                else BridgeOpened lim nbTo nbI nbFrom

-- Exemple de générateur garantissant l'invariant
genBridgeOk :: Gen IslandBridge
genBridgeOk = do
    lim <- choose (1, 100)  -- la limite initiale
    nbCars <- choose (0, lim)  -- 
    nbI <- choose (0, nbCars)
    nbTo <- choose (0, nbI)
    let nbFrom = nbCars - (nbI + nbTo)
    return $ mkBridge lim nbTo nbI nbFrom

prop_genBridgeOK_inv :: Property
prop_genBridgeOK_inv = forAll genBridgeOk $ islandBridge_inv
-- quickCheck prop_genBridgeOK_inv

bridgeSpecGenOk = do
  describe "genBridgeOk" $ do
    it "generates bridges that satisfy their invariant" $
      property prop_genBridgeOK_inv

prop_genBridgeFree_inv :: Property
prop_genBridgeFree_inv = forAll genBridgeFree $ islandBridge_inv
-- quickCheck prop_genBridgeFree_inv

bridgeSpecGenFree = do
  describe "genBridgeFree" $ do
    it "generates bridges that satisfy their invariant" $
      property prop_genBridgeFree_inv

-- Générateur par défaut
instance Arbitrary IslandBridge where
  arbitrary =
    frequency [(2, genBridgeFree) -- 20% de génération libre
              , (8, genBridgeOk)] -- 80% de génération sûre

-- quickCheck islandBridge_inv -- fails

prop_enterToIsland_inv :: IslandBridge -> Property
prop_enterToIsland_inv b =
  (islandBridge_inv b)
  && (enterToIsland_pre b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ islandBridge_inv (enterToIsland b)
-- quickCheck prop_enterToIsland_inv

enterToIslandSpec = do
  describe "enterToIsland" $ do
    it "preserves the invariant" $
      property prop_enterToIsland_inv
