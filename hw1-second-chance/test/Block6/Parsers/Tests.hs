{-# LANGUAGE ScopedTypeVariables #-}
module Block6.Parsers.Tests
  ( tests
  ) where



import Data.Maybe (isJust, isNothing)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import Block6.Parsers.Task1 (Parser(..), empty, (<|>))
import Block6.Parsers.Task2 (eof, ok, satisfy, element, stream)
import Block6.Parsers.Task3 (correctBracketSequence)
import Data.Char (ord)

tests :: IO TestTree
tests = testSpec "task 1 and 2 tests" spec

spec :: Spec
spec = do
  describe "eof tests" $ do
    it "eof on empty input" $
      runParser eof "" `shouldSatisfy` isJust
    it "eof on non-empty input" $
      runParser eof "xadsfs" `shouldSatisfy` isNothing

  describe "ok tests" $ do
    it "ok on empty input" $
      runParser ok ""  `shouldSatisfy` isJust
    it "ok on non-empty input" $
      runParser ok "afsdfad"  `shouldSatisfy` isJust

  describe "satisfy tests" $ do
    it "simple correct satisfy" $
      runParser (satisfy (== 'a')) "abc" `shouldBe` Just ('a', "bc")
    it "correct satisfy" $
      runParser (satisfy even) ([2, 3, 4] :: [Int]) `shouldBe` Just (2, [3, 4])
    it "non-correct satisfy" $
      runParser (satisfy even) ([1, 3, 4] :: [Int]) `shouldSatisfy` isNothing

  describe "element tests" $ do
    it "simple correct element" $
      runParser (element 'a') "abc" `shouldBe` Just ('a', "bc")
    it "element of int" $
      runParser (element 1) ([1, 3, 4] :: [Int]) `shouldBe` Just (1, [3, 4])
    it "non-correct element" $
      runParser (element 2) ([1, 3, 4] :: [Int]) `shouldSatisfy` isNothing

  describe "stream tests" $ do
    it "stream on empty" $
      runParser (stream "") "" `shouldBe` Just ("", "")
    it "single stream" $
      runParser (stream "a") "a" `shouldBe` Just ("a", "")
    it "single pattern on long string" $
      runParser (stream "a") "abcd" `shouldBe` Just ("a", "bcd")
    it "long pattern on long string" $
        runParser (stream "abc") "abcd" `shouldBe` Just ("abc", "d")
    it "stream of int" $
      runParser (stream [1, 2]) ([1, 2, 3, 4] :: [Int]) `shouldBe` Just ([1, 2], [3, 4])
    it "patern missmatch" $
        runParser (stream "abc") "acdb" `shouldSatisfy` isNothing
    it "too long patern" $
        runParser (stream "abcd") "acd" `shouldSatisfy` isNothing

  describe "functor instance test" $ do
    it "simple <$> test" $
      runParser (ord <$> element 'A') "A" `shouldBe` Just(65, "")
    it "complex <$> test" $
      runParser (sum <$> stream ([1, 2, 3] :: [Int])) [1,2,3] `shouldBe` Just(6, [])

  describe "applicative instance test" $ do
      it "pure test" $
        runParser (pure 'A') "A" `shouldBe` Just('A', "A")
      it "pure test on emty input" $
        runParser (pure 'A') "" `shouldBe` Just('A', "")
      it "correct <*> test" $
        runParser  ((\x y -> [x, y]) <$> element 'a' <*> element 'b') "abc" `shouldBe` Just("ab", "c")
      it "fail first <*> test" $
          runParser  ((\x y -> [x, y]) <$> element 'a' <*> element 'b') "bbc" `shouldSatisfy` isNothing
      it "fail second <*> test" $
          runParser  ((\x y -> [x, y]) <$> element 'a' <*> element 'b') "aac" `shouldSatisfy` isNothing

  describe "alternative instance test" $ do
        it "empty test" $
          runParser (empty :: Parser Char ())  "A" `shouldSatisfy` isNothing
        it "first correct <|> test" $
          runParser  (element 'a' <|> element 'b') "abc" `shouldBe` Just ('a',"bc")
        it "second correct <|> test" $
          runParser  (element 'a' <|> element 'b') "bac" `shouldBe` Just ('b',"ac")
        it "fail <|> test" $
          runParser  (element 'a' <|> element 'b') "cba" `shouldSatisfy` isNothing

  describe "monad instance test" $ do
        it "return test" $
          runParser (pure 'A') "A" `shouldBe` Just('A', "A")
        it "return test on emty input" $
          runParser (pure 'A') "" `shouldBe` Just('A', "")
        it "bind test" $
          runParser (element (1 :: Int) >>= \x -> element (x + 1)) [1, 2] `shouldBe` Just(2, [])
          
  describe "correct bracket sequence tests" $ do
        it "simple bracket" $
          runParser correctBracketSequence "()" `shouldSatisfy` isJust
        it "empty" $
          runParser correctBracketSequence "" `shouldSatisfy` isJust         
        it "nested" $
          runParser correctBracketSequence "(((())))" `shouldSatisfy` isJust
        it "sequence of correct" $
          runParser correctBracketSequence "()(((())))()()" `shouldSatisfy` isJust
        it "non-correct - too much closet" $
          runParser correctBracketSequence "())" `shouldSatisfy` isNothing
        it "non-correct - too much open" $
          runParser correctBracketSequence "()(" `shouldSatisfy` isNothing
        it "non-correct - wrong sequence" $
            runParser correctBracketSequence "())(" `shouldSatisfy` isNothing                      