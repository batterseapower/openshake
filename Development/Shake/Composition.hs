{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-} -- For the (:<) subtyping relation
module Development.Shake.Composition (
    -- * Composing namespaces
    UnionName,
    
    -- * Subtyping
    (:<), liftRule,
    
    -- * Requesting that entries get built
    need, query
  ) where

import Development.Shake.Core hiding (need)
import qualified Development.Shake.Core as Core

import Data.Binary

import Control.DeepSeq

import Control.Monad


data UnionName n1 n2 = LeftName n1 | RightName n2

fromLeftName :: UnionName n1 n2 -> Maybe n1
fromLeftName = \n -> case n of RightName _ -> Nothing; LeftName n1 -> Just n1

fromRightName :: UnionName n1 n2 -> Maybe n2
fromRightName = \n -> case n of LeftName _ -> Nothing; RightName n2 -> Just n2

instance (Namespace n1, Namespace n2) => Show (UnionName n1 n2) where
    show (LeftName n1) = show n1
    show (RightName n2) = show n2

deriving instance (Namespace n1, Namespace n2) => Eq (UnionName n1 n2)
deriving instance (Namespace n1, Namespace n2) => Ord (UnionName n1 n2)

instance (Namespace n1, Namespace n2) => NFData (UnionName n1 n2) where
    rnf (LeftName a) = rnf a
    rnf (RightName a) = rnf a

instance (Namespace n1, Namespace n2) => Binary (UnionName n1 n2) where
    get = do
        tg <- getWord8
        case tg of
          0 -> liftM LeftName get
          1 -> liftM RightName get
          _ -> error "get{UnionName n1 n2}: unknown tag"
    put (LeftName n1) = putWord8 0 >> put n1
    put (RightName n2) = putWord8 1 >> put n2


data UnionEntry n1 n2 = LeftEntry (Entry n1) | RightEntry (Entry n2)

fromLeftEntry :: UnionEntry n1 n2 -> Maybe (Entry n1)
fromLeftEntry = \n -> case n of RightEntry _ -> Nothing; LeftEntry n1 -> Just n1

fromRightEntry :: UnionEntry n1 n2 -> Maybe (Entry n2)
fromRightEntry = \n -> case n of LeftEntry _ -> Nothing; RightEntry n2 -> Just n2

deriving instance (Namespace n1, Namespace n2) => Eq (UnionEntry n1 n2)

instance (Namespace n1, Namespace n2) => Show (UnionEntry n1 n2) where
    show (LeftEntry e1) = show e1
    show (RightEntry e2) = show e2

instance (Namespace n1, Namespace n2) => NFData (UnionEntry n1 n2) where
    rnf (LeftEntry a) = rnf a
    rnf (RightEntry a) = rnf a

instance (Namespace n1, Namespace n2) => Binary (UnionEntry n1 n2) where
    get = do
        tg <- getWord8
        case tg of
          0 -> liftM LeftEntry get
          1 -> liftM RightEntry get
          _ -> error "get{UnionEntry n1 n2}: unknown tag"
    put (LeftEntry e1) = putWord8 0 >> put e1
    put (RightEntry e2) = putWord8 1 >> put e2


instance (Namespace n1, Namespace n2) => Namespace (UnionName n1 n2) where
    type Entry (UnionName n1 n2) = UnionEntry n1 n2

    sanityCheck (LeftName n1) (LeftEntry e1) = sanityCheck n1 e1
    sanityCheck (RightName n2) (RightEntry e2) = sanityCheck n2 e2
    sanityCheck _ _ = return $ Just "Mismatched name/entry structure"

    defaultRule (LeftName n1) = liftRule' (fromLeftName, fromLeftEntry) (LeftName, LeftEntry) defaultRule (LeftName n1)
    defaultRule (RightName n2) = liftRule' (fromRightName, fromRightEntry) (RightName, RightEntry) defaultRule (RightName n2)


liftRule :: (nsub :< nsup) => Rule' ntop nsub -> Rule' ntop nsup
liftRule = liftRule' downcast upcast

liftRule' :: (nsup -> Maybe nsub, Entry nsup -> Maybe (Entry nsub))
          -> (nsub -> nsup, Entry nsub -> Entry nsup)
          -> Rule' ntop nsub -> Rule' ntop nsup
liftRule' (down_name, _down_entry) (up_name, up_entry) rule ntop = case down_name ntop of
    Nothing -> return Nothing
    Just n -> liftM (fmap (\(creates, act) -> (map up_name creates, liftM (map up_entry) act))) $ rule n


class (:<) nsub nsup where
    downcast :: (nsup -> Maybe nsub, Entry nsup -> Maybe (Entry nsub))
    upcast :: (nsub -> nsup, Entry nsub -> Entry nsup) -- Stuff the two functions together to sidestep non-injectivitity of Entry

instance (:<) n n where
    downcast = (Just, Just)
    upcast = (id, id)

instance (:<) n1 (UnionName n1 n2) where
    downcast = (fromLeftName, fromLeftEntry)
    upcast = (LeftName, LeftEntry)

instance ((:<) n1 n3) => (:<) n1 (UnionName n2 n3) where
    downcast = (\n -> fromRightName n >>= name, \e -> fromRightEntry e >>= entry)
      where (name, entry) = downcast
    upcast = (RightName . name, RightEntry . entry)
      where (name, entry) = upcast


need :: forall ntop n. (n :< ntop, Namespace ntop) => [n] -> Act ntop [Entry n]
need ns = do
    top_es <- Core.need $ map up_name ns
    let Just es = mapM down_entry top_es
    return  es
  where (_down_name :: ntop -> Maybe n, down_entry) = downcast
        (up_name :: n -> ntop, _up_entry) = upcast

query :: (n :< ntop, Namespace ntop) => n -> Act ntop (Entry n)
query n = fmap (\[e] -> e) $ need [n]
