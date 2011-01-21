{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-} -- For the subtyping magic
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
type Rule o = FilePath -> Maybe (CreatesFiles, Act (ShakeName o) ())


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


defaultOracle :: StringOracle
defaultOracle = SO go
  where
    go ("ls", what) = getCurrentDirectory >>= \cwd -> globDir1 (compile what) cwd
    go question     = shakefileError $ "The default oracle cannot answer the question " ++ show question

queryStringOracle :: (String, String) -> Act (ShakeName StringOracle) [String]
queryStringOracle = fmap unSA . query . SQ

ls :: FilePath -> Act (ShakeName StringOracle) [FilePath]
ls fp = queryStringOracle ("ls", fp)


data UnionName n1 n2 = LeftName n1 | RightName n2

type ShakeName o = UnionName (Question o) CanonicalFilePath

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

instance (Namespace n1, Namespace n2) => Namespace (UnionName n1 n2) where
    type Entry (UnionName n1 n2) = UnionEntry n1 n2

    sanityCheck (LeftName n1) (LeftEntry e1) = sanityCheck n1 e1
    sanityCheck (RightName n2) (RightEntry e2) = sanityCheck n2 e2
    sanityCheck _ _ = return $ Just "Mismatched name/entry structure"
    
    defaultRule (LeftName n1) = liftLeftRule defaultRule (LeftName n1)
    defaultRule (RightName n2) = liftRightRule defaultRule (RightName n2)


-- | Attempt to build the specified files once are done collecting rules in the 'Shake' monad.
-- There is no guarantee about the order in which files will be built: in particular, files mentioned in one
-- 'want' will not necessarily be built before we begin building files in the following 'want'.
want :: (Oracle o) => [FilePath] -> Shake (ShakeName o) ()
want = act . need


(*>) :: String -> (FilePath -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: (String, CreatesFiles) -> (FilePath -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: (FilePath -> Maybe a) -> (FilePath -> a -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: (FilePath -> Bool) -> (FilePath -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act (ShakeName o) ()) -> Shake (ShakeName o) ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)



liftLeftRule :: Core.Rule' ntop n1 -> Core.Rule' ntop (UnionName n1 n2)
liftLeftRule = liftRule

liftRightRule :: forall ntop n1 n2. Core.Rule' ntop n2 -> Core.Rule' ntop (UnionName n1 n2)
liftRightRule = liftRule' fromRightName (RightName, RightEntry) -- TODO: cannot get instance search working here



liftRule :: (nsub :< nsup) => Core.Rule' ntop nsub -> Core.Rule' ntop nsup
liftRule = liftRule' downcast upcast

liftRule' :: (nsup -> Maybe nsub)
          -> (nsub -> nsup, Entry nsub -> Entry nsup)
          -> Rule' ntop nsub -> Rule' ntop nsup
liftRule' downcast upcast rule ntop = case downcast ntop of
    Nothing -> return Nothing
    Just n -> liftM (fmap (\(creates, act) -> let (name, entry) = upcast in (map name creates, liftM (map entry) act))) $ rule n


class (:<) nsub nsup where
    downcast :: nsup -> Maybe nsub
    upcast :: (nsub -> nsup, Entry nsub -> Entry nsup) -- Stuff the two functions together to sidestep non-injectivitity of Entry

instance (:<) n n where
    downcast = Just
    upcast = (id, id)

instance (:<) n1 (UnionName n1 n2) where
    downcast = fromLeftName
    upcast = (LeftName, LeftEntry)

instance ((:<) n1 n3) => (:<) n1 (UnionName n2 n3) where
    downcast n = fromRightName n >>= downcast
    upcast = (RightName . name, RightEntry . entry)
      where (name, entry) = upcast

fromLeftName :: UnionName n1 n2 -> Maybe n1
fromLeftName = \n -> case n of RightName _ -> Nothing; LeftName n1 -> Just n1

fromRightName :: UnionName n1 n2 -> Maybe n2
fromRightName = \n -> case n of LeftName _ -> Nothing; RightName n2 -> Just n2


addRule :: Rule o -> Shake (ShakeName o) ()
addRule rule = Core.addRule $ liftRightRule $ \fp -> do
    cwd <- getCurrentDirectory
    flip traverse (rule (makeRelative cwd (filePath fp))) $ \(creates, act) -> do
        creates <- mapM (canonical . (cwd </>)) creates
        return (creates, act >> mapM (liftIO . getCleanFileModTime . filePath) creates)

getCleanFileModTime :: FilePath -> IO ModTime
getCleanFileModTime fp = fmap (fromMaybe (shakefileError $ "The rule did not create a file that it promised to create " ++ fp)) $ getFileModTime fp

need :: Oracle o => [FilePath] -> Act (ShakeName o) ()
need fps = liftIO (mapM (liftM RightName . canonical) fps) >>= \fps -> Core.need fps >> return ()


installOracle :: Oracle o => o -> Shake (ShakeName o) ()
installOracle o = Core.addRule $ liftLeftRule $ \q -> return $ Just ([q], fmap return $ liftIO $ queryOracle o q)

query :: Oracle o => Question o -> Act (ShakeName o) (Answer o)
query q = do
    [LeftEntry a] <- Core.need [LeftName q]
    return a


shake :: Shake (ShakeName StringOracle) () -> IO ()
shake act = Core.shake (installOracle defaultOracle >> act)
