{-# LANGUAGE OverloadedStrings #-}

module Main where

import ParseProtherm

import Data.List.Split (splitWhen)

import qualified Data.Text as Tx
import qualified Data.Text.IO as TxIO

import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO
import Control.Monad (when)
import Data.Aeson
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as B

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
    let contents = Tx.splitOn "\n" fileContents

    -- Number lines and join continuation lines
    let lineContentsR = zipWith (,) [1..] contents
    let lineContents = joinContLines lineContentsR
    let cleanContents = (fmap . fmap) clean lineContents

    -- Break into individual entries
    let entryLines = splitWhen ((==) "//" . snd) cleanContents

    -- Attempt to parse each entry
    let results = map tryParseProtherm entryLines

    -- Display all errors
    (print . take 10 . filter (not . isSuccess)) results

    let cleanResults = (filter isJust . map fromSuccess) results

    -- Dump results
    let outPath = "ProTherm.json"
    let jsonResults = encode cleanResults
    B.writeFile outPath jsonResults


