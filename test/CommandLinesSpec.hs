{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- CommandLinesSpec.hs
-}

module CommandLinesSpec (spec) where

import Test.Hspec
import CommandLines
import qualified System.IO.Silently as Silently

manCommandSpec :: Spec
manCommandSpec = do
  describe "manCommand" $ do
    it "displays the manual" $ do
      output <- Silently.capture_ manCommand
      output `shouldContain` "Welcome to the manual !"
      output `shouldContain` "Add:"
      output `shouldContain` "example -> (+ 1 2)"
      output `shouldContain` "Sub:"
      output `shouldContain` "example -> (- 2 1)"
      output `shouldContain` "Mul:"
      output `shouldContain` "example -> (* 2 2)"
      output `shouldContain` "Div:"
      output `shouldContain` "example -> (/ 6 2)"
      output `shouldContain` "example -> (div 6 2)"
      output `shouldContain` "Eq:"
      output `shouldContain` "example -> (eq? \"test\" \"test\")"
      output `shouldContain` "Cond:"
      output `shouldContain` "example -> (if (eq? 5 4) (+ 6 6) (- 7 1))"
      output `shouldContain` "Def:"
      output `shouldContain` "example -> (define test 5)"
      output `shouldContain` "example -> (define \"test\" (+ 5 6))"

helpCommandSpec :: Spec
helpCommandSpec = do
  describe "helpCommand" $ do
    it "displays the help message" $ do
      output <- Silently.capture_ helpCommand
      output `shouldContain` "Welcome to Glados !"
      output `shouldContain` "!quit - Quit the program."
      output `shouldContain` "!man  - Display the manual."
      output `shouldContain` "!help - Display this help message."

spec :: Spec
spec = do
    manCommandSpec
    helpCommandSpec
