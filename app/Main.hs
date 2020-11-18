{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ToDo.Commands
import           ToDo.Types
import           ToDo.Parsers
import           Options.Applicative

main :: IO ()
main = do
    Options dataPath command <- execParser (info optionsParser (progDesc "To-do list manager. Please type 'todo.exe info' to list all commands"))
    run dataPath command

run :: FilePath -> Command -> IO ()
run dataPath Info = showInfo dataPath
run dataPath Init = initItems dataPath
run dataPath List = viewItems dataPath
run dataPath (Add item) = addItem dataPath item
run dataPath (View idx) = viewItem dataPath idx
run dataPath (Update idx itemUpdate) = updateItem dataPath idx itemUpdate
run dataPath (Remove idx) = removeItem dataPath idx
