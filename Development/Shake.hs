{-# LANGUAGE TypeFamilies, TypeSynonymInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, FlexibleInstances #-}
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

import Development.Shake.Core hiding (shake, addRule, need)
import qualified Development.Shake.Core as Core

import Development.Shake.Core.Binary
import Development.Shake.Core.Utilities

import Data.Binary

import Data.Typeable

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


class (Eq (Question o), Eq (Answer o),
       Ord (Question o),
       Binary (Question o), Binary (Answer o),
       Show (Question o), Show (Answer o),       -- Show is only required for nice debugging output
       NFData (Question o), NFData (Answer o),   -- NFData is only required for reasonable errors when deserialization fails
       Typeable o) => Oracle o where
    data Question o
    data Answer o
    queryOracle :: o -> Question o -> IO (Answer o)


newtype StringOracle = SO ((String, String) -> IO [String])
                     deriving (Typeable)

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


defaultOracle :: StringOracle
defaultOracle = SO go
  where
    go ("ls", what) = getCurrentDirectory >>= \cwd -> globDir1 (compile what) cwd
    go question     = shakefileError $ "The default oracle cannot answer the question " ++ show question

queryStringOracle :: (String, String) -> Act (ShakeName StringOracle) [String]
queryStringOracle = fmap unSA . query . SQ

ls :: FilePath -> Act (ShakeName StringOracle) [FilePath]
ls fp = queryStringOracle ("ls", fp)


data ShakeName o = File CanonicalFilePath
                 | OracleQuestion (Question o)

instance Oracle o => Show (ShakeName o) where
    show (File fp) = show fp
    show (OracleQuestion q) = show q

deriving instance Oracle o => Eq (ShakeName o)
deriving instance Oracle o => Ord (ShakeName o)

instance Oracle o => NFData (ShakeName o) where
    rnf (File a) = rnf a
    rnf (OracleQuestion a) = rnf a

instance Oracle o => Binary (ShakeName o) where
    get = do
        tg <- getWord8
        case tg of
          0 -> liftM File get
          1 -> liftM OracleQuestion get
          _ -> error "get{ShakeName o}: unknown tag"
    put (File fp) = putWord8 0 >> put fp
    put (OracleQuestion q) = putWord8 1 >> put q


data ShakeEntry o = FileModTime ModTime
                  | OracleAnswer (Answer o)

deriving instance Oracle o => Eq (ShakeEntry o)

instance Oracle o => Show (ShakeEntry o) where
    show (FileModTime mtime) = show mtime
    show (OracleAnswer a) = show a

instance Oracle o => NFData (ShakeEntry o) where
    rnf (FileModTime a) = rnf a
    rnf (OracleAnswer a) = rnf a

instance Oracle o => Binary (ShakeEntry o) where
    get = do
        tg <- getWord8
        case tg of
          0 -> liftM FileModTime get
          1 -> liftM OracleAnswer get
          _ -> error "get{ShakeEntry o}: unknown tag"
    put (FileModTime mtime) = putWord8 0 >> put mtime
    put (OracleAnswer a) = putWord8 1 >> put a


instance Oracle o => Namespace (ShakeName o) where
    type Entry (ShakeName o) = ShakeEntry o
    -- In standard Shake, we would:
    --   * Not sanity check anything
    --   * Recover the ModTime by looking at the file modification time
    --   * And hence dependent files would be rebuilt but the file would not be, even if the modification time had changed since the last run
    --
    -- In OpenShake, we would:
    --   * Cache the ModTime
    --   * Sanity check the current ModTime against the current one
    --   * Thus we detect changes in this file since the last run, so changed files will be rebuilt even if their dependents haven't changed
    sanityCheck (OracleQuestion _) _ = return Nothing -- No way to sanity check oracle question without running it. TODO: check type?
    sanityCheck (File fp) (FileModTime old_mtime) = getFileModTime (filePath fp) >>= \mb_new_mtime -> return $ guard (mb_new_mtime /= Just old_mtime) >> Just "the file has been modified (or deleted) even though its dependents have not changed"
     -- TODO: how to deal with apparently non-exhaustive patterns like this one?
    
    defaultRule (OracleQuestion _) = return Nothing -- No default way to answer oracle questions
    defaultRule (File fp) = do
        -- Not having a rule might still be OK, as long as there is some existing file here:
        mb_nested_time <- getFileModTime (filePath fp)
        case mb_nested_time of
            Nothing          -> return Nothing
            -- NB: it is important that this fake oracle is not reachable if we change from having a rule for a file to not having one,
            -- but the file still exists. In that case we will try to recheck the old oracle answers against our new oracle and the type
            -- check will catch the change.
            Just nested_time -> return $ Just ([File fp], return [FileModTime nested_time]) -- TODO: distinguish between files created b/c of rule match and b/c they already exist in history? Lets us rebuild if the reason changes.


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


addRule :: Rule o -> Shake (ShakeName o) ()
addRule rule = Core.addRule go
  where
    go (OracleQuestion _) = return Nothing
    go (File fp) = do
      cwd <- getCurrentDirectory
      flip traverse (rule (makeRelative cwd (filePath fp))) $ \(creates, act) -> do
          creates <- mapM (canonical . (cwd </>)) creates
          return (map File creates, act >> mapM (liftM FileModTime . liftIO . getCleanFileModTime . filePath) creates)

getCleanFileModTime :: FilePath -> IO ModTime
getCleanFileModTime fp = fmap (fromMaybe (shakefileError $ "The rule did not create a file that it promised to create " ++ fp)) $ getFileModTime fp

need :: Oracle o => [FilePath] -> Act (ShakeName o) ()
need fps = liftIO (mapM (liftM File . canonical) fps) >>= \fps -> Core.need fps >> return ()


installOracle :: Oracle o => o -> Shake (ShakeName o) ()
installOracle o = Core.addRule go
  where
    go (File _) = return Nothing
    go (OracleQuestion q) = return $ Just ([OracleQuestion q], fmap (return . OracleAnswer) $ liftIO $ queryOracle o q)

query :: Oracle o => Question o -> Act (ShakeName o) (Answer o)
query q = do
    [OracleAnswer a] <- Core.need [OracleQuestion q]
    return a


shake :: Shake (ShakeName StringOracle) () -> IO ()
shake act = Core.shake (installOracle defaultOracle >> act)
