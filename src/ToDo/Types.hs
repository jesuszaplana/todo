{-# LANGUAGE DeriveGeneric #-}

module ToDo.Types where

import           Data.Time
import qualified Data.Yaml as Yaml
import           GHC.Generics

defaultDataPath :: FilePath
defaultDataPath = "~/.todo.yaml"

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance Yaml.ToJSON Priority
instance Yaml.FromJSON Priority

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance Yaml.ToJSON ToDoList
instance Yaml.FromJSON ToDoList

data Item = Item
    { title         :: ItemTitle
    , description   :: ItemDescription
    , priority      :: ItemPriority
    , dueBy         :: ItemDueBy
    } deriving (Generic,Show)
instance Yaml.ToJSON Item
instance Yaml.FromJSON Item

data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy
    } deriving Show

data Options = Options FilePath Command
    deriving Show

data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show
