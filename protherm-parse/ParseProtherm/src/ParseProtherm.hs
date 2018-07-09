{-# LANGUAGE OverloadedStrings #-}
module ParseProtherm where

  import Data.Text as Tx
  import Data.Attoparsec.Text as P
  
  import qualified Data.Map as Map
--  import Debug.Trace (trace)
  import Data.Char (isSpace)
  import Control.Applicative ((<|>))

  import Data.Aeson (toJSON)
  import Data.Aeson.Types (ToJSON)

  type Unit = Text  -- E.g. meters, pounds, millimolar
  type Label = Text

  data PTValue = VEmpty
               | VText Text
               | VNum Double
               | VRelated [Int]
               | VId Int
                 deriving (Show, Ord, Eq)

  instance ToJSON PTValue where
    toJSON (VText a) = toJSON a
    toJSON (VNum a) = toJSON a
    toJSON (VRelated a) = toJSON a
    toJSON (VId a) = toJSON a
    toJSON VEmpty = ""

  type PTEntry = Map.Map Label PTValue

  getName :: Parser Label
  getName = P.takeWhile (not . isSpace)

  getVText :: Parser Text
  getVText = P.takeWhile isSpace >> P.takeTill isEndOfLine

  getVNum :: Parser Double
  getVNum = double <* endOfInput

  vRelH :: Parser [Int]
  vRelH = getVRelated <|> return []

  getVRelated :: Parser [Int]
  getVRelated = do
    first <- decimal
    _ <- takeWhile1 (== ',')
    rest <- vRelH
    return (first : rest)

  getVId :: Parser Int
  getVId = do
    x <- decimal
    endOfInput
    return x

  parseLine :: Parser (Label, PTValue)
  parseLine = do
    label <- getName
    skipSpace
    value <- (P.takeWhile isSpace >> endOfInput >> return VEmpty)
             <|> fmap VRelated getVRelated
             <|> fmap VNum getVNum
             <|> fmap VText getVText
             <|> fmap VId getVId
    return (label, value)

  tryParseLine :: Text -> Either Text (Label, PTValue)
  tryParseLine x = 
   case parseOnly parseLine x of
    Left msg -> Left $ Tx.pack msg
    Right result -> Right result

  addLine :: Text -> Either Text PTEntry -> Either Text PTEntry
  addLine _ (Left err) = Left err
  addLine line (Right kvmap) = case tryParseLine line of
    (Left err) -> Left err
    (Right (name, val)) -> Right $ Map.insert name val kvmap

  parseEntry :: [Text] -> Either Text PTEntry
  parseEntry = Prelude.foldr addLine (Right Map.empty)

  joinContinuationLines :: [Text] -> [Text]
  joinContinuationLines []  = []
  joinContinuationLines [x] = [x]
  joinContinuationLines (x:y:xs) = 
    if isSpace $ Tx.head y
    then Tx.append x (clean y) : joinContinuationLines xs
    else joinContinuationLines (y:xs)
    where clean = Tx.dropWhile isSpace
