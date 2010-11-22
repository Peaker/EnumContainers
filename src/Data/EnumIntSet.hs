module Data.EnumIntSet(EnumIntSet, empty, union,
                       unions, fromList, toList, elems,
                       member, notMember) where

import Prelude hiding (lookup)
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.Monoid(Monoid(..))

newtype EnumIntSet k = EnumIntSet { unEnumIntSet :: IntSet }
  deriving (Show, Read, Eq, Ord)

inEnumIntSet :: (IntSet -> IntSet) -> EnumIntSet k -> EnumIntSet k'
inEnumIntSet f = EnumIntSet . f . unEnumIntSet

inEnumIntSet2 :: (IntSet -> IntSet -> IntSet) -> EnumIntSet k -> EnumIntSet k' -> EnumIntSet k''
inEnumIntSet2 f = inEnumIntSet . f . unEnumIntSet

instance Monoid (EnumIntSet k) where
  mempty = EnumIntSet mempty
  mappend = inEnumIntSet2 mappend

empty :: Enum k => EnumIntSet k
empty = mempty

union :: EnumIntSet k -> EnumIntSet k -> EnumIntSet k
union = mappend

unions :: [EnumIntSet k] -> EnumIntSet k
unions = mconcat

fromList :: Enum k => [k] -> EnumIntSet k
fromList = EnumIntSet . IntSet.fromList . map fromEnum

toList :: Enum k => EnumIntSet k -> [k]
toList = map toEnum . IntSet.toList . unEnumIntSet

elems :: Enum k => EnumIntSet k -> [k]
elems = toList

member :: Enum k => k -> EnumIntSet k -> Bool
member k = IntSet.member (fromEnum k) . unEnumIntSet

notMember :: Enum k => k -> EnumIntSet k -> Bool
notMember k = not . member k
