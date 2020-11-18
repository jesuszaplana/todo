module ToDo.Commands where

import           ToDo.Types
import           Control.Exception
import           Control.Monad
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List.Safe ((!!))
import           Data.Time
import qualified Data.Yaml as Yaml
import           Prelude hiding ((!!))
import           System.Console.ANSI
import           System.Directory
import           System.IO.Error

writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

-- readToDoList :: FilePath -> IO (Maybe ToDoList)
-- readToDoList dataPath = BS.readFile dataPath >>= return . Yaml.decode

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "YAML file is corrupt"
        Just toDoList -> return toDoList

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Nothing -> print "index does not exists"
        Just xs -> showItem idx xs

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
    putStrLn $ "[" ++ show idx ++ "]: " ++ title
    putStr "     Description: "
    putStrLn $ showField id mbDescription
    putStr "     Priority:    "
    putStrLn $ showField show mbPriority
    putStr "     Due by:      "
    putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing  = "(not set)"

viewItems :: FilePath -> IO ()
viewItems dataPath = do
    ToDoList items <- readToDoList dataPath
    forM_
        (zip [0..] items)
        (\(idx, item) -> showItem idx item)

showInfo :: FilePath -> IO ()
showInfo dataPath = do
    putStrLn $ "Data file path: " ++ dataPath
    exists <- doesFileExist dataPath
    if exists
    then do
        s <- BS.readFile dataPath
        let mbToDoList = Yaml.decode s
        case mbToDoList of
            Nothing -> putStrLn $ "Status: file is invalid"
            Just (ToDoList items) -> putStrLn $ "Status: contains " ++ show (length items) ++ " items"
    else putStrLn $ "Status: file does not exist"

initItems :: FilePath -> IO ()
initItems dataPath = writeToDoList dataPath (ToDoList [])

addItem :: FilePath -> Item -> IO ()
addItem dataPath item = do
    ToDoList items <- readToDoList dataPath
    let newTodoList = ToDoList (item : items)
    writeToDoList dataPath newTodoList

removeItem :: FilePath -> ItemIndex -> IO ()
removeItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItems = items `removeAt` idx
    case mbItems of
        Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Invalid item index"
            setSGR [Reset]
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList

removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            _ : after' = after
            xs' = before ++ after'
        in Just xs'

updateItem :: FilePath -> ItemIndex -> ItemUpdate -> IO ()
updateItem dataPath idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
    ToDoList items <- readToDoList dataPath
    let update (Item title description priority dueBy) = Item
            (updateField mbTitle title)
            (updateField mbDescription description)
            (updateField mbPriority priority)
            (updateField mbDueBy dueBy)
        updateField (Just value) _ = value
        updateField Nothing value = value
        mbItems = updateAt items idx update
    case mbItems of
        Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Invalid item index"
            setSGR [Reset]
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList

updateAt :: [a] -> Int -> (a -> a) -> Maybe [a]
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            element : after' = after
            xs' = before ++ f element : after'
        in Just xs'
