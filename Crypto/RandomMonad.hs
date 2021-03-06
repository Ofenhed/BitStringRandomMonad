{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}

module Crypto.RandomMonad (RndT, RndST, RndIO, Rnd, RndState, getRandomM, getRandom2M, runRndT, newRandomElementST, getRandomElement, randomElementsLength, replaceSeedM, addSeedM, getRandomByteStringM, RandomElementsListST, RndStateList(..), RndStateParallelization(..), BitStringToRandomExceptions(..), seedFromBytestrings, seedFromBytestringsThreaded, seedFromBytestringsM) where

import Control.DeepSeq (deepseq, force)
import Control.Exception (Exception, throw)
import Control.Monad.Identity (Identity)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Lazy (StateT, put, state, runStateT)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parMap, rpar, using, rseq)
import Data.Bits (shiftL, (.|.), xor)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Typeable (Typeable)

import qualified Data.BitString as BS
import qualified Data.ByteString as ByS
import qualified Data.ByteString.Lazy as ByLS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

data BitStringToRandomExceptions = OutOfElementsException deriving (Show, Typeable)
data RandomElementsListST a s = VM.Unbox a => RandomElementsListST (STRef s (V.Vector a))
instance Exception BitStringToRandomExceptions

bitsNeeded :: Integer -> Integer
bitsNeeded x = (+) 1 $ floor $ logBase 2 (fromIntegral x)

convertBitStringToInteger = BS.foldl' convert' 0
  where
  convert' :: Integer -> Bool -> Integer
  convert' prev cur = (shiftL prev 1) .|. (case cur of True -> 1 ; False -> 0)

multipleBitstringsSplitAt i (RndStateList p [x]) = let (takers, droppers) = BS.splitAt i x in ([takers], RndStateList p [droppers])

multipleBitstringsSplitAt i (RndStateList RndStateParallel x) = join' (split' x) [] []
 where
 split' = parMap rpar (\bs -> let (take,drop) = BS.splitAt i bs in (take `using` rseq, drop))
 join' [] takers droppers = (takers, RndStateList RndStateParallel droppers)
 join' (x:xs) takers droppers = let (newTake, newDrop) = x in join' xs (newTake:takers) (newDrop:droppers)

multipleBitstringsSplitAt i (RndStateList p x) = join' (split' x) [] []
 where
 split' = map $ BS.splitAt i
 join' [] takers droppers = (takers, RndStateList p droppers)
 join' (x:xs) takers droppers = let (newTake, newDrop) = x in join' xs (newTake:takers) (newDrop:droppers)

multipleBitstringsAssertLength _ [] = False
multipleBitstringsAssertLength len x = len' x
  where
  len' [] = True
  len' (x:xs) = if (BS.length x) == len
                   then len' xs
                   else False


getRandom :: Integer -> RndStateList -> (Integer, RndStateList)
getRandom 0 x = (0, x)
getRandom max string = if has_error
                          then error "There was an error acquiring random data"
                          else if random <= max
                                  then (random, unused)
                                  else getRandom max unused
  where
    bitsNeeded' = bitsNeeded max
    has_error = not $ multipleBitstringsAssertLength (fromInteger bitsNeeded') used
    random = foldl (\i cur -> xor i $ convertBitStringToInteger cur) 0 used
    (used, unused) = multipleBitstringsSplitAt (fromIntegral bitsNeeded') string

getRandom2 :: Integer -> Integer -> RndStateList -> (Integer, RndStateList)
getRandom2 a b string = getRandom2' (getRandom (max' - min') string)
  where
  min' = min a b
  max' = max a b
  getRandom2' (random, unused) = (random + min', unused)

getRandomByteString :: Integer -> RndStateList -> (ByS.ByteString, RndStateList)
getRandomByteString 0 x = (ByS.pack [], x)
getRandomByteString len x = let (byte, newState) = getRandom 255 x ; (allBytes, lastState) = getRandomByteString (len - 1) newState in (ByS.cons (fromIntegral byte) allBytes, lastState)

newRandomElementST :: VM.Unbox a => [a] -> ST s (RandomElementsListST a s)
newRandomElementST acc = (newSTRef $ V.fromList acc) >>= \ref -> return $ RandomElementsListST ref

getRandomElement :: (RandomElementsListST a s) -> RndST s a
getRandomElement (RandomElementsListST ref) = do
  mut <- lift $ readSTRef ref >>= V.unsafeThaw
  let n = toInteger $ VM.length mut
  j <- if n > 0 then getRandomM $ n - 1
                else throw OutOfElementsException
  let j' = fromInteger j
  lift $ do
    aa <- VM.read mut 0
    ab <- VM.read mut j'
    VM.write mut j' aa
    vec <- V.unsafeFreeze mut
    writeSTRef ref $ V.unsafeTail vec
    return ab

randomElementsLength :: RandomElementsListST a s -> RndST s Int
randomElementsLength (RandomElementsListST ref) = do
  vec <- lift $ readSTRef ref
  return $ V.length vec

type RndStatePrimitive = [BS.BitString]
data RndStateParallelization = RndStateSeq | RndStatePrecalculate | RndStateParallel
data RndStateList = RndStateList RndStateParallelization RndStatePrimitive

type RndState = RndStateList
newtype RndT m a = RndT
  { unRndT :: StateT RndState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance PrimMonad m => PrimMonad (RndT m) where
  type PrimState (RndT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

type RndST s a = RndT (ST s) a
type RndIO a = RndT IO a
type Rnd a = RndT Identity a

replaceSeedM :: Monad m => RndStatePrimitive -> RndT m ()
replaceSeedM s = RndT $ state $ replaceSeedM s
  where
  replaceSeedM newState (RndStateList p _) = ((),RndStateList p newState)

addSeedM :: Monad m => RndStatePrimitive -> RndT m ()
addSeedM s = RndT $ state $ addSeedM s
  where
  addSeedM x (RndStateList p y) = ((),RndStateList p (x ++ y))


getRandomM :: Monad m => Integer -> RndT m Integer
getRandomM x = RndT $ state $ getRandom x

getRandom2M :: Monad m => Integer -> Integer -> RndT m Integer
getRandom2M x y = RndT $ state $ getRandom2 x y

getRandomByteStringM :: Monad m => Integer -> RndT m ByS.ByteString
getRandomByteStringM x = RndT $ state $ getRandomByteString x

runRndT :: RndState -> RndT m a -> m (a, RndState)
runRndT rnd m = runStateT (unRndT m) rnd

withOneElementPrecalculated (x:xs@(y:_)) = par (force y) x :
                                           withOneElementPrecalculated xs

seedFromBytestrings :: [ByS.ByteString] -> RndStatePrimitive
seedFromBytestrings list = [BS.bitStringLazy $ ByLS.fromChunks list]

seedFromBytestringsThreaded :: [ByS.ByteString] -> RndStatePrimitive
seedFromBytestringsThreaded list = [BS.bitStringLazy $ ByLS.fromChunks $ withOneElementPrecalculated list]

seedFromBytestringsM :: Monad m => [ByS.ByteString] -> RndT m RndStatePrimitive
seedFromBytestringsM list = RndT $ state $ getSeed list
  where
  getSeed list st@(RndStateList RndStatePrecalculate _) = (seedFromBytestringsThreaded list, st)
  getSeed list st@(RndStateList _ _) = (seedFromBytestrings list, st)
