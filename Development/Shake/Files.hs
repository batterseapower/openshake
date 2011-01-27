{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Development.Shake.Files (
    -- * File modification times
    ModTime, getFileModTime,
    
    -- * Normalised file paths
    CanonicalFilePath, canonical,
    
    -- * Rules for building files
    CreatesFiles, Rule,
    (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    
    -- * Requesting that files get built
    need, want
  ) where

import Development.Shake.Core hiding (Rule, addRule, need)
import qualified Development.Shake.Core as Core
import Development.Shake.Core.Binary
import Development.Shake.Core.Utilities
import Development.Shake.Composition hiding (need)
import qualified Development.Shake.Composition as Composition

import Data.Binary

import Control.DeepSeq

import Control.Monad
import Control.Monad.IO.Class

import Data.Traversable (Traversable(traverse))

import System.Directory
import System.FilePath (takeDirectory, equalFilePath, makeRelative, (</>))
import System.FilePath.Glob
import System.Time (ClockTime(..))


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


data CanonicalFilePath = UnsafeCanonicalise { originalFilePath :: FilePath, canonicalFilePath :: FilePath }

instance Show CanonicalFilePath where
    show = originalFilePath -- TODO: confirm that Show is only used in errors, and use Pretty instead?

instance Eq CanonicalFilePath where
    cfp1 == cfp2 = canonicalFilePath cfp1 `equalFilePath` canonicalFilePath cfp2

instance Ord CanonicalFilePath where
    cfp1 `compare` cfp2 = canonicalFilePath cfp1 `compare` canonicalFilePath cfp2

instance NFData CanonicalFilePath where
    rnf (UnsafeCanonicalise a b) = rnf a `seq` rnf b

instance Binary CanonicalFilePath where
    get = liftM2 UnsafeCanonicalise getUTF8String getUTF8String
    put (UnsafeCanonicalise fp1 fp2) = putUTF8String fp1 >> putUTF8String fp2


canonical :: FilePath -> IO CanonicalFilePath
canonical fp = do
    exists <- doesFileExist fp
    if exists
      then fmap (UnsafeCanonicalise fp) $ canonicalizePath fp
      else fmap (UnsafeCanonicalise fp . (</> fp)) getCurrentDirectory


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
    sanityCheck fp old_mtime = getFileModTime (canonicalFilePath fp) >>= \mb_new_mtime -> return $ guard (mb_new_mtime /= Just old_mtime) >> Just "the file has been modified (or deleted) even though its dependents have not changed"
    
    defaultRule fp = do
        -- Not having a rule might still be OK, as long as there is some existing file here:
        mb_nested_time <- getFileModTime (canonicalFilePath fp)
        case mb_nested_time of
            Nothing          -> return Nothing
            -- NB: it is important that this fake oracle is not reachable if we change from having a rule for a file to not having one,
            -- but the file still exists. In that case we will try to recheck the old oracle answers against our new oracle and the type
            -- check will catch the change.
            Just nested_time -> return $ Just ([fp], return [nested_time]) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.


type CreatesFiles = [FilePath]
type Rule ntop o = FilePath -> Maybe (CreatesFiles, Act ntop ())

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
    flip traverse (rule (makeRelative cwd (canonicalFilePath fp))) $ \(creates, act) -> do
        creates <- mapM (canonical . (cwd </>)) creates
        return (creates, mapM_ (liftIO . createDirectoryIfMissing True . takeDirectory . canonicalFilePath) creates >> act >> mapM (liftIO . getCleanFileModTime . canonicalFilePath) creates)
  where
    getCleanFileModTime :: FilePath -> IO ModTime
    getCleanFileModTime fp = getFileModTime fp >>= maybe (shakefileError $ "The rule did not create a file that it promised to create " ++ fp) return


need :: (CanonicalFilePath :< ntop, Namespace ntop) => [FilePath] -> Act ntop ()
need fps = liftIO (mapM canonical fps) >>= Composition.need >> return ()

-- | Attempt to build the specified files once are done collecting rules in the 'Shake' monad.
-- There is no guarantee about the order in which files will be built: in particular, files mentioned in one
-- 'want' will not necessarily be built before we begin building files in the following 'want'.
want :: (CanonicalFilePath :< ntop, Namespace ntop) => [FilePath] -> Shake ntop ()
want = act . need