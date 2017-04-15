import Crypto.RandomMonad (runRndT, getRandomM, RndStateList(RndStateListSequencial, RndStateListParallel), getRandomByteStringM)

import Control.Monad.ST (runST)
import Test.QuickCheck
import Data.BitString as BiS
import Data.ByteString.Lazy.Char8 as LC8
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

prop_parallelEquals1 = do
  let lists = [BiS.bitStringLazy $ LC8.pack $ Prelude.concat $ Prelude.repeat "Hej, jag loopar här",
               BiS.bitStringLazy $ LC8.pack $ Prelude.concat $ Prelude.repeat "Hej, jag loopar också här"]
      (first, _) = runST $ runRndT (RndStateListSequencial lists) $ getRandomByteStringM 9001
      (second, _) = runST $ runRndT (RndStateListParallel lists) $ getRandomByteStringM 9001
   in first == second

main = exitWith $ if prop_parallelEquals1 then ExitSuccess else ExitFailure 1
