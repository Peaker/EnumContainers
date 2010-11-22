module Data.EnumIntMap(EnumIntMap, lookup, findWithDefault, (!), fromList, fromListBy, keys, elems, toList, empty, elementsFromMap) where

import Prelude hiding (lookup)
import Control.Arrow(first, (&&&))
import Data.Maybe(fromMaybe)
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import Data.Monoid(Monoid(..))
import Control.DeepSeq (NFData, rnf)

newtype EnumIntMap k v = EnumIntMap { unEnumIntMap :: IntMap v }
  deriving (Show, Read, Eq, Ord)

inEnumIntMap :: (IntMap v -> IntMap v') -> EnumIntMap k v -> EnumIntMap k v'
inEnumIntMap f = EnumIntMap . f . unEnumIntMap

inEnumIntMap2 :: (IntMap v -> IntMap v' -> IntMap v'') -> EnumIntMap k v -> EnumIntMap k v' -> EnumIntMap k v''
inEnumIntMap2 f = inEnumIntMap . f . unEnumIntMap

instance (Enum a, NFData b) => NFData (EnumIntMap a b) where
  rnf = rnf . unEnumIntMap

instance Monoid (EnumIntMap k v) where
  mempty = EnumIntMap mempty
  mappend = inEnumIntMap2 mappend

empty :: Enum k => EnumIntMap k v
empty = mempty

lookup :: Enum k => k -> EnumIntMap k v -> Maybe v
k `lookup` EnumIntMap intMap = IntMap.lookup (fromEnum k) intMap

findWithDefault :: Enum k => v -> k -> EnumIntMap k v -> v
findWithDefault v k (EnumIntMap intMap) = IntMap.findWithDefault v (fromEnum k) intMap

(!) :: Enum k => EnumIntMap k v -> k -> v
em ! k = fromMaybe (error "EnumIntMap.! no such key") $ lookup k em

fromList :: Enum k => [(k, v)] -> EnumIntMap k v
fromList = EnumIntMap . IntMap.fromList . (map . first) fromEnum

fromListBy :: Enum k => (v -> k) -> [v] -> EnumIntMap k v
fromListBy f = fromList . map (f &&& id)

toList :: Enum k => EnumIntMap k v -> [(k, v)]
toList = (map . first) toEnum . IntMap.toList . unEnumIntMap

elems :: Enum k => EnumIntMap k v -> [v]
elems = map snd . toList

keys :: Enum k => EnumIntMap k v -> [k]
keys = map fst . toList

elementsFromMap :: (Enum k) => [k] -> EnumIntMap k b -> [b]
elementsFromMap ks m = map (m !) ks
