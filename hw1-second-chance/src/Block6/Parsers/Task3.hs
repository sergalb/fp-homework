module Block6.Parsers.Task3
  ( correctBracketSequence
  ) where

import Block6.Parsers.Task1 (Parser (..), (<|>))
import Block6.Parsers.Task2 (element, eof, ok)

-- | Parser witch accept correct braket sequence
-- return () on success
correctBracketSequence :: Parser Char ()
correctBracketSequence = partCorrect *> eof

partCorrect :: Parser Char ()
partCorrect = (element '(' *> partCorrect *> element ')' *> partCorrect) <|> ok
