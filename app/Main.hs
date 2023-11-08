{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Task1
import qualified Task2

main :: IO ()
main = do
    putStrLn "--- 1 ---"
    Task1.main
    putStrLn "--- 2 ---"
    Task2.main
