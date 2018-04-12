{-# LANGUAGE OverloadedStrings #-}

module Main where

import ParseProtherm

import qualified Data.Text as Tx
import qualified Data.Text.IO as TxIO

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import qualified Data.Either as E
import qualified Data.Map as Map

import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO
import Control.Monad (when)


outputFName :: FilePath
outputFName = "ProTherm.json"

main :: IO ()
main = do
    args <- getArgs
    when (null args || length args > 1) $ do 
        pn <- getProgName
        putStrLn $ "Usage: " ++ pn ++ " <path-to-protherm.dat>"
        exitWith (ExitFailure 1)

    -- Open file and read in contents to a Data.Text
    let filePath = args !! 0
    h <- openFile filePath ReadMode
    hSetEncoding h char8
    fileContents <- TxIO.hGetContents h
    let contents = (fmap (Tx.splitOn "\n") . Tx.splitOn "//\n") fileContents
    let cleanContents = (fmap) (filter (not . Tx.null)) contents
    let entries = (fmap (filter (\x -> Tx.take 5 x /= "*****")) . 
                   fmap (filter (\x -> Tx.length x /= 0)) ) cleanContents

    let parsedEntries = map parseEntry entries

    -- Show errors
    mapM_ print $ E.lefts parsedEntries
    
    -- Write outputs
    let outputs1 = E.rights parsedEntries
    let outputs2 = map (Map.filter ((/=) VEmpty)) outputs1

    B.writeFile outputFName $ encode outputs2

    -- Print error messages from 

    return ()