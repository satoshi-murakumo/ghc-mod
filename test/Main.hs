{-# LANGUAGE CPP, ScopedTypeVariables #-}
import Spec
import Dir

import Control.Exception as E
import Control.Monad (void, forM_)
import Language.Haskell.GhcMod (debugInfo)
import System.Process
import Test.Hspec
import TestUtils
----
import Data.List (isPrefixOf)
import System.Directory (removeFile)

main :: IO ()
main = do
  let sandboxes = [ "test/data", "test/data/check-packageid"
                  , "test/data/duplicate-pkgver/"
                  , "test/data/broken-cabal/"
                  ]
      genSandboxCfg dir = withDirectory dir $ \cwdir -> do
          content <- fmap (unlines . map (replace "@CWD@" cwdir) . lines) $ readFile "cabal.sandbox.config.in"
          writeFile "cabal.sandbox.config" content
      pkgDirs =
        [ "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/check-packageid/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/duplicate-pkgver/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
      genGhcPkgCache dir = system $ "ghc-pkg recache --force -f" ++ dir
  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs
  forM_ ["test/setup-config", "test/ghc-mod.cache"] $ E.handle ignore . removeFile
  void $ system "cabal --version"
  putStrLn $ "ghc-mod was built with Cabal version " ++ VERSION_Cabal
  void $ system "ghc --version"

  (putStrLn =<< runD debugInfo) `E.catch` ignore

  hspec spec


replace :: Eq t => [t] -> [t] -> [t] -> [t]
replace _ _ [] = []
replace a b str@(s:ss)
  | a `isPrefixOf` str = b ++ drop (length a) str
  | otherwise        = s : replace a b ss

ignore :: E.SomeException -> IO ()
ignore _ = return ()
