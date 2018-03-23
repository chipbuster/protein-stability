{-# LANGUAGE OverloadedStrings #-}
module ParseProtherm where

  import System.IO
  import Data.Text
  import Data.Text.ICU
  import qualified Data.Map as Map

  -- Utility functions for unit-dependent data
  data Concentration = ConcmM Double | ConcM Double deriving (Read, Show, Eq)
  data Temperature = TempC Double | TempK Double deriving (Read, Show, Eq)


  toC :: Temperature -> Double
  toC (TempC x) = x
  toC (TempK x) = 273.15 + x

  tomM :: Concentration -> Double
  tomM (ConcmM x) = x
  tomM (ConcM x) = 1000.0 * x

  -- Utility functions for percentages
  data Percent = Percent Double | Decimal Double deriving (Read, Show, Eq)

  toDec :: Percent -> Double
  toDec (Percent x) = 0.01 * x
  toDec (Decimal x) = x

  data SSInf = SSInf { name :: Text
                     , source :: Text
                     , length :: Integer
                     , weight :: Double
                     , other :: Map.Map Text Text
  }

  data ExpCond = ExpCond { measure :: Text
                         , method :: Text
                         , temp :: Maybe Temperature
                         , names :: Map.Map Text Text
                         , concs :: Map.Map Text Concentration
                         }
  
  data Lit = Lit { kwords :: [Text]
                 , reference :: Text
                 , author :: [Text]
                 , remarks :: [Text]
                 , related :: [Integer]
  }

  data ThermData = ThermData { reversibility :: Maybe Text
                   , state :: Maybe Integer
                   , activities :: Map.Map Text Percent
                   , consts :: Map.Map Text Double
                   }

  data PTEntry = Entry {
                        id :: Integer 
                       ,ssInfo :: SSInf 
                       ,expCond :: ExpCond 
                       ,thermData :: ThermData
                       ,litInfo :: Lit
                       }

