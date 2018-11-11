module System.Plugins.Dynloader where

import GHC hiding (ModuleName, load)
import GHC.Paths (libdir)
import DynFlags (PackageFlag(..), PackageArg(..), ModRenaming(..))
import Data.Dynamic (Dynamic)
import Unsafe.Coerce

unsafeLoad 
  :: [PackageName]  -- ^ e.g. vector
  -> [ModuleName] -- ^ e.g. Data.Vector
  -> Expression -- ^ e.g. length
  -> IO a
unsafeLoad pkgs mdls exp = runGhcT (Just libdir) $ do
  initDynFlags
  setContext $ map (IIDecl . simpleImportDecl . mkModuleName) mdls
  r <- compileExpr exp
  return $ unsafeCoerce r
  where
    initDynFlags = do
      df <- getSessionDynFlags
      setSessionDynFlags df {
        hscTarget = HscInterpreted,
        ghcLink = LinkInMemory,
        packageFlags = [ExposePackage p (PackageArg p) (ModRenaming True []) | p <- pkgs]
        }

type PackageName = String
type ModuleName = String
type Expression = String
