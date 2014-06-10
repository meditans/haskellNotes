import           Control.Monad

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (get)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
  return w # set title "Ciao"
  buttonAndList <- mkButtonAndList
  getBody w #+ map element buttonAndList

-- Introduciamo delle nuove funzioni che derivano dall'interfaccia di Reactive
-- (<$) :: Functor f => a -> f b -> f a
-- questa deriva da applicative, e serve a sostituire ogni istanza del funtore
-- con lo stesso valore. Ad esempio: 3 <$ [1,2,4] = [3,3,3].
-- Quindi la spiegazione intuitiva e' che stia sostituendo il click. Ma vediamo
-- il tipo:
-- UI.click :: Element -> Event ()
-- allora si sostituisce Event () in maniera da avere un Event (Int -> Int)?. Vediamo!
-- In effetti e' vero. Dobbiamo immaginarlo come una lista in cui in alcuni punti accadono
-- delle funzioni

-- Il punto e' che quello strano tipo e' quello che serve alla funzione accumB
-- accumB :: MonadIO m => a -> Event (a -> a) -> m (Behavior a)
-- accumB sta per accumulate Behaviour, e in effetti si comporta come un foldl.
-- quindi parte da 0, e va aggiungendo uno ogni volta! Alla fine si comporta come
-- uno stepper, guardare la documentazione.

-- infine, abbiamo
-- (<@) :: Behavior a -> Event b -> Event a
-- Quindi <@ prende un behavior, e usa i tempi di Event b per dire cos'e'
-- in quei tempi Event a

mkButtonAndList :: UI [Element]
mkButtonAndList = do
  myButton <- UI.button # set text "Click me!"
  myList <- UI.ul

  let eClick = UI.click myButton
      eIncrement = (+(1::Int)) <$ eClick
  bCounter <- accumB 0 eIncrement

  let eCount = bCounter <@ eClick

  onEvent eCount $ \count -> element myList #+ [UI.li # set text (show count)]
  return [myButton, myList]

-- Ricapitoliamo. registro il click, ci cambio dentro il tipo mettendoci una funzione
-- e alla fine lo updato ad un behaviour con accumB. Infine, lo samplo quando mi serve
-- per rispondere agli eventi del mouse.

-- Notare il pattern generale, che viene fatto notare su StackOverflow.

-- Inoltre leggere la guida per il widget design, anche se non c'e' molto!
