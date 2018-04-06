{-# LANGUAGE OverloadedStrings #-}
module ParseProtherm where

  import System.IO
  import Data.Text as Tx
  import Data.Text.ICU as Re
  import qualified Data.Map as Map
  import Data.Maybe (fromJust, isJust)

  type PTEntry = Map.Map Text Text

  type LineData = (Integer, Text)

  data ParseResult a = FatalError Text
                     | Error Text
                     | Success a deriving (Show, Eq)

  instance Functor ParseResult where
    fmap f (FatalError x) = FatalError x
    fmap f (Error x) = Error x
    fmap f (Success s) = Success (f s)

  instance Applicative ParseResult where
    pure = Success
    _ <*> (FatalError x) = FatalError x
    (FatalError x) <*> _ = FatalError x
    Error x <*> _ = Error x
    _ <*> Error x = Error x
    (Success f) <*> (Success x) = Success (f x)

  instance Monad ParseResult where
    r1 >>= f = case r1 of
      (FatalError x) -> FatalError x
      (Error x) -> Error x
      (Success s) -> f s

    return = pure

  emptyPattern = regex [] "([A-Z_]+).*"          -- Match line with no data
  linePattern = regex [] "([A-Z_\\.]+) +([^\\s].*)" -- Match line with data
  headerPattern = regex [] "\\*+ .* \\*+"         -- Match section header

  -- Clean off trailing commas and any leading/trailing whitespace
  clean :: Text -> Text
  clean = strip . dropWhileEnd (==',')

  -- Attempt to parse a line and add to an existing PTEntry
  tryParseLine :: LineData -> ParseResult PTEntry -> ParseResult PTEntry
  tryParseLine (line, text) ptentry = 
    let lineMatchM = Re.find linePattern text 
    in if isJust lineMatchM
      {- If it matches an important line, parse the data -}
       then let match = fromJust lineMatchM 
           in if groupCount match /= 2 
              then FatalError $ constructBadMatchMessage (line, text)
              else let get n = fromJust . Re.group n
                   in Map.insert (get 1 match) (get 2 match) <$> ptentry

       {- If it matches a non-important line, we can just return an empty match.
          Otherwise, it matches something unexcpected and an error should be
          raised -}
       else if isJust (Re.find emptyPattern text) || isJust (Re.find headerPattern text)
            then return Map.empty
            else FatalError $ constructBadMatchMessage (line,text)

  -- Attempt to parse a full protherm entry
  tryParseProtherm :: [LineData] -> ParseResult PTEntry
  tryParseProtherm = Prelude.foldr tryParseLine (return Map.empty)

  constructBadMatchMessage :: LineData -> Text
  constructBadMatchMessage (line, text) = 
    "Error on line " `mappend` Tx.pack (show line) `mappend` ": could not match"
    `mappend` "line to any known pattern, line was " `mappend` text