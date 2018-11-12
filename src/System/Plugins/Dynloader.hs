module System.Plugins.Dynloader (
  unsafeLoad,
  load
  ) where

import GHC hiding (ModuleName, load)
import GHC.Paths (libdir)
import DynFlags (PackageFlag(..), PackageArg(..), ModRenaming(..))
import Data.Dynamic (Dynamic)
import Unsafe.Coerce

-- |
-- e.g.
--
-- @
-- f <- unsafeload [] ["Prelude"] "(+)"
-- f 1 2 -- 3
-- @
unsafeLoad :: [PackageName] -> [ModuleName] -> Expression -> IO a
unsafeLoad pkgs mdls exp = runGhcT (Just libdir) $ do
  initGhc pkgs mdls
  r <- compileExpr exp
  return $ unsafeCoerce r

load :: [PackageName] -> [ModuleName] -> Expression -> IO Dynamic
load pkgs mdls exp = runGhcT (Just libdir) $ do
  initGhc pkgs mdls
  dynCompileExpr exp

type PackageName = String
type ModuleName = String
type Expression = String

initGhc :: GhcMonad m => [PackageName] -> [ModuleName] -> m ()
initGhc pkgs mdls = do
  initDynFlags
  setContext $ map (IIDecl . simpleImportDecl . mkModuleName) mdls
  where
    initDynFlags = do
      df <- getSessionDynFlags
      setSessionDynFlags df {
        hscTarget = HscInterpreted,
        ghcLink = LinkInMemory,
        packageFlags = [ExposePackage p (PackageArg p) (ModRenaming True []) | p <- pkgs]
        }
