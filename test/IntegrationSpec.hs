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
      output `shouldBe` "Exception: variable foo not found (* \"foo\" 2)\n"

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

testOne :: Spec
testOne = do
  describe "firsttest" $ do
    it "correctly evaluates the example from examples/test1.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test1.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "12\n"

ifone :: Spec
ifone = do
  describe "if2" $ do
    it "correctly evaluates the example from examples/if1.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "if1.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "1\n"

iftwo :: Spec
iftwo = do
  describe "if2" $ do
    it "correctly evaluates the example from examples/if2.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "if2.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "2\n"

spec :: Spec
spec = do
    callTestSpec
    errorTestSpec
    fooTestSpec
    testOne
    ifone
    iftwo
