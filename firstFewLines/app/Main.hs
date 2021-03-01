module Main where

dataPath = "../data.csv"

main :: IO ()
main =
    do
    raw <- readFile "../data.csv"
    writeFile "100lines.csv" $ unlines $ take 1000 $ lines raw
