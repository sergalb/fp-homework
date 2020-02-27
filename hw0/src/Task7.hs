{-# LANGUAGE ScopedTypeVariables #-}

module Task7 
  ( firstExpression
  ) where

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