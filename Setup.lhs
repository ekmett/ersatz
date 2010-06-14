#!/usr/bin/env runhaskell

TODO: update the po/kata.pot and any procedural .po files here?

\begin{code}
{-# LANGUAGE CPP #-}

{-
import Control.Monad ( zipWithM_, when, unless, filterM )
import Data.Version( showVersion )
import Distribution.Text ( display )
import Distribution.System (OS(Windows), buildOS)
import Distribution.Version (Version(versionBranch))
import Distribution.Package ( packageVersion, packageName, PackageName(..) )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription, cppOptions, ccOptions
         , library, libBuildInfo, otherModules )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup (buildVerbosity, copyDest, copyVerbosity, fromFlag, haddockVerbosity, installVerbosity, sDistVerbosity)
import Distribution.Simple.Utils (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout, rewriteFile)
import Distribution.Verbosity ( Verbosity )
import System.Directory
    (copyFile, createDirectory, createDirectoryIfMissing,
     doesDirectoryExist, doesFileExist,
     getCurrentDirectory, getDirectoryContents,
     removeDirectoryRecursive, removeFile, setCurrentDirectory)
-}
import GHC.IOBase (ExitCode(..))
import Control.Monad (unless)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) ) -- , absoluteInstallDirs, externalPackageDeps )

import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks) 
import System.IO (openFile, IOMode (..))
import System.FilePath ( (</>), splitDirectories, isAbsolute )
import System.Process 

main = defaultMainWithHooks simpleUserHooks { runTests = testHook }

testHook args _ _ lbi = do
    let args' = if null args then [] else "-t" : args
    exitcode <- system (unwords ((buildDir lbi </> "test-ersatz" </> "test-ersatz") : args')) 
    unless (exitcode == ExitSuccess) $ 
        fail "Test failed"
    

\end{code}
