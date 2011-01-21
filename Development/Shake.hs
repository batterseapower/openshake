{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    want, act, oracle, modifyOracle,
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
    -- * The monadic interface used by rule bodies
    Act, need, query,
    
    -- * Namespaces
    CanonicalFilePath, -- TODO: as an alternative, I could newtype the Shake/Act monads
    
    -- * Oracles, the default oracle and wrappers for the questions it can answer
    Oracle(..), StringOracle(..), defaultOracle, stringOracle, queryStringOracle, ls,
    
    -- * Used to add commands to the shake report
    reportCommand
  ) where

import Development.Shake.Core hiding (addRule, need)
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
type Rule o = FilePath -> Maybe (CreatesFiles, Act CanonicalFilePath o ())


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
            Just nested_time -> return $ Just ([fp], GeneratorAct () $ return [nested_time]) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.


canonical :: FilePath -> IO CanonicalFilePath
canonical fp = do
    exists <- doesFileExist fp
    if exists
      then fmap UnsafeCanonicalise $ canonicalizePath fp
      else fmap (UnsafeCanonicalise . (</> fp)) getCurrentDirectory


-- | Attempt to build the specified files once are done collecting rules in the 'Shake' monad.
-- There is no guarantee about the order in which files will be built: in particular, files mentioned in one
-- 'want' will not necessarily be built before we begin building files in the following 'want'.
want :: (Oracle o) => [FilePath] -> Shake CanonicalFilePath o ()
want = act . need


(*>) :: Oracle o => String -> (FilePath -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(*>) pattern action = (compiled `match`) ?> action
  where compiled = compile pattern

(*@>) :: Oracle o => (String, CreatesFiles) -> (FilePath -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(*@>) (pattern, alsos) action = (\fp -> guard (compiled `match` fp) >> return alsos) ?@> action
  where compiled = compile pattern

(**>) :: Oracle o => (FilePath -> Maybe a) -> (FilePath -> a -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(**>) p action = addRule $ \fp -> p fp >>= \x -> return ([fp], action fp x)

(**@>) :: Oracle o => (FilePath -> Maybe ([FilePath], a)) -> (FilePath -> a -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(**@>) p action = addRule $ \fp -> p fp >>= \(creates, x) -> return (creates, action fp x)

(?>) :: Oracle o => (FilePath -> Bool) -> (FilePath -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(?>) p action = addRule $ \fp -> guard (p fp) >> return ([fp], action fp)

(?@>) :: Oracle o => (FilePath -> Maybe CreatesFiles) -> (FilePath -> Act CanonicalFilePath o ()) -> Shake CanonicalFilePath o ()
(?@>) p action = addRule $ \fp -> p fp >>= \creates -> return (creates, action fp)


addRule :: Oracle o => Rule o -> Shake CanonicalFilePath o ()
addRule rule = Core.addRule $ \o fp -> do
    cwd <- getCurrentDirectory
    flip traverse (rule (makeRelative cwd (filePath fp))) $ \(creates, act) -> do
        creates <- mapM (canonical . (cwd </>)) creates
        return (creates, GeneratorAct o $ act >> mapM (liftIO . getCleanFileModTime . filePath) creates)

getCleanFileModTime :: FilePath -> IO ModTime
getCleanFileModTime fp = fmap (fromMaybe (shakefileError $ "The rule did not create a file that it promised to create " ++ fp)) $ getFileModTime fp

need :: [FilePath] -> Act CanonicalFilePath o ()
need fps = liftIO (mapM canonical fps) >>= \fps -> Core.need fps >> return ()
