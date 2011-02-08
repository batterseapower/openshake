{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances #-} -- For the (:<) subtyping relation
module Development.Shake.Composition (
    -- * Composing namespaces
    (:+:), Empty,
    
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


-- | Union together two namespaces.
--
-- Trees of (:+:) must be right-biased, or the subtyping machinery won't be able to infer
-- the subtype relationship.
data (:+:) n1 n2 = LeftName n1 | RightName n2

infixr 1 :+: -- Ensure right-biased construction by default

fromLeftName :: n1 :+: n2 -> Maybe n1
fromLeftName = \n -> case n of RightName _ -> Nothing; LeftName n1 -> Just n1

fromRightName :: n1 :+: n2 -> Maybe n2
fromRightName = \n -> case n of LeftName _ -> Nothing; RightName n2 -> Just n2

instance (Namespace n1, Namespace n2) => Show (n1 :+: n2) where
    show (LeftName n1) = show n1
    show (RightName n2) = show n2

deriving instance (Namespace n1, Namespace n2) => Eq (n1 :+: n2)
deriving instance (Namespace n1, Namespace n2) => Ord (n1 :+: n2)

instance (Namespace n1, Namespace n2) => NFData (n1 :+: n2) where
    rnf (LeftName a) = rnf a
    rnf (RightName a) = rnf a

instance (Namespace n1, Namespace n2) => Binary (n1 :+: n2) where
    get = do
        tg <- getWord8
        case tg of
          0 -> liftM LeftName get
          1 -> liftM RightName get
          _ -> error "get{(:+:) n1 n2}: unknown tag"
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


instance (Namespace n1, Namespace n2) => Namespace (n1 :+: n2) where
    type Entry (n1 :+: n2) = UnionEntry n1 n2

    sanityCheck (LeftName n1) (LeftEntry e1) = sanityCheck n1 e1
    sanityCheck (RightName n2) (RightEntry e2) = sanityCheck n2 e2
    sanityCheck _ _ = return $ Just "Mismatched name/entry structure"

    defaultRule (LeftName n1) = liftRule' (fromLeftName, fromLeftEntry) (LeftName, LeftEntry) defaultRule (LeftName n1)
    defaultRule (RightName n2) = liftRule' (fromRightName, fromRightEntry) (RightName, RightEntry) defaultRule (RightName n2)
    
    data Snapshot (n1 :+: n2) = UnionSnapshot (Snapshot n1) (Snapshot n2)
    
    takeSnapshot = liftM2 UnionSnapshot takeSnapshot takeSnapshot
    lintSnapshots building_ns sss = lintSnapshots building_ns1 [(ss1, ss1', fst (partitionNames ns)) | (UnionSnapshot ss1 _ss2, UnionSnapshot ss1' _ss2', ns) <- sss] ++ lintSnapshots building_ns2 [(ss2, ss2', snd (partitionNames ns)) | (UnionSnapshot _ss1 ss2, UnionSnapshot _ss1' ss2', ns) <- sss]
      where (building_ns1, building_ns2) = partitionNames building_ns

partitionNames :: [n1 :+: n2] -> ([n1], [n2])
partitionNames ns = ([n1 | LeftName n1 <- ns], [n2 | RightName n2 <- ns])


-- | It is occasionally useful to have a "unit" namespace that is a subtype of everything. There are no (non-bottom) names of this type.
data Empty

deriving instance Eq Empty
deriving instance Ord Empty
deriving instance Show Empty

instance Binary Empty where
    get = return (error "Forced a deserialized Empty thunk")
    put _ = return ()

instance NFData Empty

instance Namespace Empty where
    type Entry Empty = Empty
    data Snapshot Empty = EmptySnapshot
    takeSnapshot = return EmptySnapshot
    lintSnapshots _ _ = []


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

-- Due to limitations of the Haskell inference machinery, we implement right-biased
-- search of namespaces composed with (:+:). Left branches match only by direct unification:
instance (:<) n1 (n1 :+: n2) where
    downcast = (fromLeftName, fromLeftEntry)
    upcast = (LeftName, LeftEntry)

-- We do full search in the right hand subtree:
instance ((:<) n1 n3) => (:<) n1 (n2 :+: n3) where
    downcast = (\n -> fromRightName n >>= name, \e -> fromRightEntry e >>= entry)
      where (name, entry) = downcast
    upcast = (RightName . name, RightEntry . entry)
      where (name, entry) = upcast

-- This is a more "experimental" instance that gives us full "width subtyping". We *have* to expand
-- the second parameter to (n3 :+: n4) or it ambiguously overlaps with the rule above. Luckily this
-- doesn't make the rule less applicable, because you shouldn't be trying to make (n1 :+: n2) a subtype
-- of a non-(:+:) type. The only case where that would even make sense is (n1 == n2), which is degenerate.
instance ((:<) n1 (n3 :+: n4), (:<) n2 (n3 :+: n4)) => (:<) (n1 :+: n2) (n3 :+: n4) where
    downcast = (\n -> fmap LeftName (name1 n) `mplus` fmap RightName (name2 n), \e -> fmap LeftEntry (entry1 e) `mplus` fmap RightEntry (entry2 e))
      where (name1, entry1) = downcast
            (name2, entry2) = downcast
    upcast = (\n -> case n of LeftName n1 -> name1 n1; RightName n2 -> name2 n2, \e -> case e of LeftEntry e1 -> entry1 e1; RightEntry e2 -> entry2 e2)
      where (name1, entry1) = upcast
            (name2, entry2) = upcast

instance (:<) Empty n where
    downcast = (const Nothing, const Nothing)
    upcast = (\_ -> error "Forced an upcasted Empty", \_ -> error "Forced an upcasted Empty entry")


need :: forall ntop n. (n :< ntop, Namespace ntop) => [n] -> Act ntop [Entry n]
need ns = do
    top_es <- Core.need $ map up_name ns
    let Just es = mapM down_entry top_es
    return  es
  where (_down_name :: ntop -> Maybe n, down_entry) = downcast
        (up_name :: n -> ntop, _up_entry) = upcast

query :: (n :< ntop, Namespace ntop) => n -> Act ntop (Entry n)
query n = fmap (\[e] -> e) $ need [n]
