import Test.Hspec

import RevrevSpec as RR
import SplitSpec as SP
import BridgeSpec as BS

main :: IO ()
main = hspec $ do
  -- revrev
  RR.revrevSpec
  -- split
  SP.splitSpec0
  SP.splitSpec1
  SP.splitSpec2
  SP.splitSpec3
  -- bridge
  BS.bridgeSpecInit
  BS.bridgeSpecGenOk
  BS.bridgeSpecGenFree
  BS.enterToIslandSpec
