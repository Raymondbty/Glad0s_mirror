{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- GladosSpec.hs
-}

module IntegrationSpec (spec) where

import System.Process
import System.Exit (ExitCode(ExitSuccess))
import System.Directory
import System.FilePath ((</>))
import Test.Hspec

defaultFilePath :: FilePath -> IO FilePath
defaultFilePath envVar = do
  currentDir <- getCurrentDirectory
  let filename = envVar
  return $ currentDir </> "test" </> "examples" </> filename

callTestSpec :: Spec
callTestSpec = do
  describe "call" $ do
    it "correctly evaluates the example from examples/call.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "call.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "5\n"

errorTestSpec :: Spec
errorTestSpec = do
  describe "error" $ do
    it "correctly evaluates the example from examples/error.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "error.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "variable foo not found\n"

fooTestSpec :: Spec
fooTestSpec = do
  describe "foo" $ do
    it "correctly evaluates the example from examples/foo.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "foo.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "42\n"

spec :: Spec
spec = do
    callTestSpec
    errorTestSpec
    fooTestSpec
