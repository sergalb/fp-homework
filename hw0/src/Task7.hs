{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( firstExpression
  , secondExpression
  ) where


import Data.Either (lefts, rights)

-- | Full annotated expression
firstExpression :: Bool
firstExpression = (($) :: ([String] -> Bool) -> [String] -> Bool) nullDotHead annotatedMap
  where
    (nullDotHead :: [String] -> Bool) =
      ((.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool)
      (null :: String -> Bool)
      (head :: [String] -> String)
    (annotatedMap :: [String]) =
      (map :: ((String -> String, String) -> String) -> [(String -> String, String)] -> [String])
        (uncurryId :: (String -> String, String) -> String)
        (list :: [(String -> String, String)])
        where
          uncurryId = (uncurry :: ((String -> String) -> String -> String) -> (String -> String, String) -> String)
                          (id :: (String -> String) -> String -> String)
          list = ((:) :: (String -> String, String) -> [(String -> String, String)] -> [(String -> String, String)])
                   (pairCreator :: (String -> String, String))
                   ([] :: [(String -> String, String)])
                   where
                     pairCreator =
                       ((,) :: (String -> String) -> String -> ((String -> String), String))
                         (((++) "Dorian ") :: String -> String)
                         (" Grey" :: String)




-- | Full annotated expression
secondExpression :: [(Integer, Integer)]
secondExpression = lambda
                  (((:) :: Either Integer Integer -> [Either Integer Integer] ->[Either Integer Integer])
                    (Left onePlusTwo :: Either Integer Integer)
                    (((:) :: Either Integer Integer -> [Either Integer Integer] ->[Either Integer Integer])
                      (Right twoPowerSix :: Either Integer Integer)
                      ([] :: [Either Integer Integer])))
                      where
                        onePlusTwo = ((+) :: Integer -> Integer -> Integer) (1 :: Integer) (2 :: Integer)
                        twoPowerSix = ((^) :: Integer -> Integer -> Integer) (2 :: Integer) (6 :: Integer)

lambda :: [Either Integer Integer] -> [(Integer, Integer)]
lambda (x :: [Either Integer Integer]) = (zip :: [Integer] -> [Integer] -> [(Integer, Integer)])
                                          (((lefts :: [Either Integer Integer] -> [Integer])  x) :: [Integer])
                                          (((rights :: [Either Integer Integer] -> [Integer])  x) :: [Integer])