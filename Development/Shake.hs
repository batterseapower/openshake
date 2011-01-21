-- | Top-level module that exports a commonly-used subset of the Shake API
module Development.Shake (
    -- * The top-level monadic interface
    Shake, shake,
    
    -- * Adding file rules in the Shake monad
    Rule, CreatesFiles, (*>), (*@>), (**>), (**@>), (?>), (?@>), addRule,
    
    -- * Demanding files and other things in the Shake monad
    want, act,
    
    -- * The monadic interface used by rule bodies
    Act, need, query,
    
    -- * Oracle definition
    Oracle(..), installOracle,
    
    -- * The file system oracle, and wrappers for the questions it can answer
    FileSystemOracle, ls,
    
    -- * Namespaces
    Namespace(..), (:<),
    
    -- * The file namespace
    CanonicalFilePath, -- TODO: as an alternative, I could newtype the Shake/Act monads?
    
    -- * Verbosity and command-line output from Shake
    Verbosity(..), actVerbosity, putStrLnAt,
    
    -- * Adding to the Shake report
    reportCommand
  ) where

import Development.Shake.Core hiding (Rule, shake, addRule, need)
import qualified Development.Shake.Core as Core

import Development.Shake.Composition hiding (need)
import Development.Shake.Files
import Development.Shake.Oracles


shake :: Shake (UnionName (Question FileSystemOracle) CanonicalFilePath) () -> IO () -- TODO: make ntop polymorphic
shake act = Core.shake ({- FIXME: installOracle defaultOracle >> -} act)
