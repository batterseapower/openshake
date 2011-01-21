{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-} -- For the subtyping magic
{-# LANGUAGE ViewPatterns #-}
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    want, act, installOracle,
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
    -- * The monadic interface used by rule bodies
    Act, need, query,
    
    -- * Namespaces
    Namespace(..), (:<),
    CanonicalFilePath, -- TODO: as an alternative, I could newtype the Shake/Act monads
    
    -- * Oracles, the default oracle and wrappers for the questions it can answer
    Oracle(..), StringOracle(..), defaultOracle, queryStringOracle, ls,
    
    -- * Used to add commands to the shake report
    reportCommand
  ) where

import Development.Shake.Core hiding (Rule, shake, addRule, need)
import qualified Development.Shake.Core as Core

import Development.Shake.Core.Binary
import Development.Shake.Core.Utilities

import Data.Binary

import Control.DeepSeq

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.Traversable (Traversable(traverse))

import System.Directory
import System.FilePath (equalFilePath, makeRelative, (</>))
import System.FilePath.Glob
import System.Time (ClockTime(..))


type CreatesFiles = [FilePath]
type Rule ntop o = FilePath -> Maybe (CreatesFiles, Act ntop ())


type ModTime = ClockTime

-- TODO: remove orphan instance
instance NFData ClockTime where
    rnf = rnfModTime

-- TODO: remove orphan instance
instance Binary ClockTime where
    get = getModTime
    put = putModTime

rnfModTime :: ModTime -> ()
rnfModTime (TOD a b) = rnf a `seq` rnf b

getModTime :: Get ModTime
getModTime = liftM2 TOD get get

putModTime :: ModTime -> Put
putModTime (TOD a b) = put a >> put b

getFileModTime :: FilePath -> IO (Maybe ModTime)
getFileModTime fp = handleDoesNotExist (return Nothing) (fmap Just (getModificationTime fp))


newtype CanonicalFilePath = UnsafeCanonicalise { filePath :: FilePath }
                          deriving (Ord, NFData)

instance Show CanonicalFilePath where
    show = filePath -- TODO: confirm that Show is only used in errors, and use Pretty instead?

instance Eq CanonicalFilePath where
    cfp1 == cfp2 = filePath cfp1 `equalFilePath` filePath cfp2

instance Binary CanonicalFilePath where
    get = fmap UnsafeCanonicalise getUTF8String
    put = putUTF8String . filePath


canonical :: FilePath -> IO CanonicalFilePath
canonical fp = do
    exists <- doesFileExist fp
    if exists
      then fmap UnsafeCanonicalise $ canonicalizePath fp
      else fmap (UnsafeCanonicalise . (</> fp)) getCurrentDirectory


class Namespace (Question o) => Oracle o where
    data Question o
    data Answer o
    queryOracle :: o -> Question o -> IO (Answer o)


newtype StringOracle = SO ((String, String) -> IO [String])

instance Oracle StringOracle where
    newtype Question StringOracle = SQ { unSQ :: (String, String) }
                                  deriving (Eq, Ord, Show, NFData)
    newtype Answer StringOracle = SA { unSA :: [String] }
                                deriving (Eq, Show, NFData)
    queryOracle (SO f) = fmap SA . f . unSQ

instance Binary (Question StringOracle) where
    get = fmap SQ $ liftM2 (,) getUTF8String getUTF8String
    put (SQ (x, y)) = putUTF8String x >> putUTF8String y

instance Binary (Answer StringOracle) where
    get = fmap SA $ getList getUTF8String
    put = putList putUTF8String . unSA

instance Oracle o => Namespace (Question o) where
    type Entry (Question o) = Answer o

    sanityCheck _ _ = return Nothing -- No way to sanity check oracle question without running it
    defaultRule _ = return Nothing -- No default way to answer oracle questions


data UnionName n1 n2 = LeftName n1 | RightName n2

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


instance Namespace CanonicalFilePath where
    type Entry CanonicalFilePath = ModTime
    
    -- In standard Shake, we would:
    --   * Not sanity check anything
    --   * Recover the ModTime by looking at the file modification time
    --   * And hence dependent files would be rebuilt but the file would not be, even if the modification time had changed since the last run
    --
    -- In OpenShake, we would:
    --   * Cache the ModTime
    --   * Sanity check the current ModTime against the current one
    --   * Thus we detect changes in this file since the last run, so changed files will be rebuilt even if their dependents haven't changed
    sanityCheck fp old_mtime = getFileModTime (filePath fp) >>= \mb_new_mtime -> return $ guard (mb_new_mtime /= Just old_mtime) >> Just "the file has been modified (or deleted) even though its dependents have not changed"
    
    defaultRule fp = do
        -- Not having a rule might still be OK, as long as there is some existing file here:
        mb_nested_time <- getFileModTime (filePath fp)
        case mb_nested_time of
            Nothing          -> return Nothing
            -- NB: it is important that this fake oracle is not reachable if we change from having a rule for a file to not having one,
            -- but the file still exists. In that case we will try to recheck the old oracle answers against our new oracle and the type
            -- check will catch the change.
            Just nested_time -> return $ Just ([fp], return [nested_time]) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.


liftRule :: (nsub :< nsup) => Core.Rule' ntop nsub -> Core.Rule' ntop nsup
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

fromLeftName :: UnionName n1 n2 -> Maybe n1
fromLeftName = \n -> case n of RightName _ -> Nothing; LeftName n1 -> Just n1

fromRightName :: UnionName n1 n2 -> Maybe n2
fromRightName = \n -> case n of LeftName _ -> Nothing; RightName n2 -> Just n2

fromLeftEntry :: UnionEntry n1 n2 -> Maybe (Entry n1)
fromLeftEntry = \n -> case n of RightEntry _ -> Nothing; LeftEntry n1 -> Just n1

fromRightEntry :: UnionEntry n1 n2 -> Maybe (Entry n2)
fromRightEntry = \n -> case n of LeftEntry _ -> Nothing; RightEntry n2 -> Just n2


-- Needing and adding rules for files

(*>) :: (CanonicalFilePath :< ntop, Namespace ntop)
     => String -> (FilePath -> Act ntop ()) -> Shake ntop ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: (CanonicalFilePath :< ntop, Namespace ntop)
      => (String, CreatesFiles) -> (FilePath -> Act ntop ()) -> Shake ntop ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: (CanonicalFilePath :< ntop, Namespace ntop)
      => (FilePath -> Maybe a) -> (FilePath -> a -> Act ntop ()) -> Shake ntop ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: (CanonicalFilePath :< ntop, Namespace ntop)
       => (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act ntop ()) -> Shake ntop ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: (CanonicalFilePath :< ntop, Namespace ntop)
     => (FilePath -> Bool) -> (FilePath -> Act ntop ()) -> Shake ntop ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: (CanonicalFilePath :< ntop, Namespace ntop)
      => (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act ntop ()) -> Shake ntop ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)

addRule :: (CanonicalFilePath :< ntop, Namespace ntop) => Rule ntop o -> Shake ntop ()
addRule rule = Core.addRule $ liftRule $ \fp -> do
    cwd <- getCurrentDirectory
    flip traverse (rule (makeRelative cwd (filePath fp))) $ \(creates, act) -> do
        creates <- mapM (canonical . (cwd </>)) creates
        return (creates, act >> mapM (liftIO . getCleanFileModTime . filePath) creates)
  where
    getCleanFileModTime :: FilePath -> IO ModTime
    getCleanFileModTime fp = fmap (fromMaybe (shakefileError $ "The rule did not create a file that it promised to create " ++ fp)) $ getFileModTime fp

need :: (CanonicalFilePath :< ntop, Namespace ntop) => [FilePath] -> Act ntop ()
need fps = liftIO (mapM (liftM (fst upcast) . canonical) fps) >>= \fps -> Core.need fps >> return ()

-- | Attempt to build the specified files once are done collecting rules in the 'Shake' monad.
-- There is no guarantee about the order in which files will be built: in particular, files mentioned in one
-- 'want' will not necessarily be built before we begin building files in the following 'want'.
want :: (CanonicalFilePath :< ntop, Namespace ntop) => [FilePath] -> Shake ntop ()
want = act . need


-- Needing (query) and adding rules (installing) for oracles

installOracle :: (Oracle o, Question o :< ntop) => o -> Shake ntop ()
installOracle o = Core.addRule $ liftRule $ \q -> return $ Just ([q], fmap return $ liftIO $ queryOracle o q)

query :: forall ntop o. (Oracle o, Question o :< ntop, Namespace ntop) => Question o -> Act ntop (Answer o)
query q = do
    [down_entry -> Just a] <- Core.need [up_name q]
    return a
  where (_down_name :: ntop -> Maybe (Question o), down_entry) = downcast
        (up_name :: Question o -> ntop, _up_entry) = upcast

queryStringOracle :: (Question StringOracle :< ntop, Namespace ntop) => (String, String) -> Act ntop [String]
queryStringOracle = fmap unSA . query . SQ

ls :: (Question StringOracle :< ntop, Namespace ntop) => FilePath -> Act ntop [FilePath]
ls fp = queryStringOracle ("ls", fp)


shake :: Shake (UnionName (Question StringOracle) CanonicalFilePath) () -> IO () -- TODO: make ntop polymorphic
shake act = Core.shake (installOracle defaultOracle >> act)

defaultOracle :: StringOracle
defaultOracle = SO go
  where
    go ("ls", what) = getCurrentDirectory >>= \cwd -> globDir1 (compile what) cwd
    go question     = shakefileError $ "The default oracle cannot answer the question " ++ show question
