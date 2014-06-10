import           Control.Concurrent.STM
import           Control.Monad               (void)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core


type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String

main :: IO ()
main = do
  database <- atomically $ newTVar Map.empty
  startGUI defaultConfig (setup database)

setup :: TVar Database -> Window -> UI ()
setup database rootWindow = void $ do
  userNameInput <- UI.input # set (attr "placeholder") "User Name"
  loginButton <- UI.button #+ [string "Login"]
  getBody rootWindow #+ map element [userNameInput, loginButton]

  on UI.click loginButton $ \_ -> do
    username <- get value userNameInput
    currentItems <- fmap Set.toList $ liftIO $ atomically $ do
      db <- readTVar database
      case Map.lookup username db of
        Nothing -> do
          writeTVar database (Map.insert username Set.empty db)
          return Set.empty
        Just items -> return items

    let showItem item = UI.li #+ [string item]
    toDoContainer <- UI.ul #+ map showItem currentItems

    newItem <- UI.input

    on UI.sendValue newItem $ \input -> do
      liftIO $ atomically $ modifyTVar database $
        Map.adjust (Set.insert input) username

      set UI.value "" (element newItem)
      element toDoContainer #+ [showItem input]

    header <- UI.h1 #+ [string $ username ++ "'s todo list"]
    set children [header, toDoContainer, newItem] (getBody rootWindow)
