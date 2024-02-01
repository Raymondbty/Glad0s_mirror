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

callTestOne :: Spec
callTestOne = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_one.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_one.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "x: -5 !\ntest\n\n4\n"

callTestTwo :: Spec
callTestTwo = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_two.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_two.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "Exception: stack overflow: test2();\n"

callTestThree :: Spec
callTestThree = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_three.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_three.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "3\n260\n34\n530\n3\n"

callTestFour :: Spec
callTestFour = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_four.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_four.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "3\n3\n4\n"

callTestFive :: Spec
callTestFive = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_five.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_five.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "2\n4\n"

callTestFact :: Spec
callTestFact = do
  describe "call" $ do
    it "correctly evaluates the example from examples/test_factorial.scm" $ do
      exePath <- makeAbsolute "./glados"
      inputPath <- defaultFilePath "test_factorial.scm"
      inputContent <- readFile inputPath
      (exitCode, output, _) <- readProcessWithExitCode exePath [] inputContent
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "3628800\n"

spec :: Spec
spec = do
    callTestOne
    callTestTwo
    callTestThree
    callTestFour
    callTestFive
    callTestFact
