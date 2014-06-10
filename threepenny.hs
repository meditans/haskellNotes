import           Control.Concurrent          (threadDelay)
import           Control.Monad

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

-- startGUI :: Config -> (Window -> UI ()) -> IO () --defined in Graphics.UI.Threepenny.Core
-- defaultConfig :: Config                          --defined in Graphics.UI.Threepenny.Core

-- Cerchiamo di capire cosa esprimono questi tipi

-- Config                                           --defined in Graphics.UI.Threepenny.Core

-- Window                                           --defined in Graphics.UI.Threepenny.Core
-- type Window = Session
-- the client browser windows

-- UI ()
-- User interface elements are created and manipulated in the UI monad.
-- Questo e' importante: Creati e manipolati nella monade UI
-- This monad is essentially just a thin wrapper around the familiar IO monad.
-- (use the liftIO function to access IO operations) like reading and writing from files.
-- Further reasons to use: more convenience when calling JavaScript and
-- recursion for functional reactive programming.

main :: IO ()
main = startGUI defaultConfig setup

-- void :: Functor f => f a -> f () -- Control.Monad
-- praticamente cancella il contenuto di un funtore, mantenendone la forma :O
-- cosa vuol dire se cancella il contenuto da IO? Cioe' se ne frega del risultato
-- per fare tornare il risultato giusto.
-- Ricordiamo che gli elementi vengono manipolati in UI (). Per questo devo portare
-- w in UI prima di applicare set

-- set :: ReadWriteAttr x i o -> i -> UI x -> UI x
-- readWriteAttr e' un attributo generico con diversi tipi di getter e setter
-- visto che implementa le funzioni (get' :: x -> UI o) e (set' :: i -> x -> UI ())
-- ne deduciamo che si riferisce alla x, puo' restituire qualcosa di tipo o, e settare
-- a partire da un elemento di tipo i. Le lettere i e o stanno probabilmente per input
-- e output

-- title :: WriteAttr Window String
-- Per quanto riguarda la classe WriteAttr, WriteAttr x i = ReadWriteAttr x i (),
-- ovvero e' un attributo che supporta unicamente il setting. Cosi' capiamo che
-- title e' un attributo che puo' settare qualcosa in una finestra del browser usando
-- una stringa.

-- Vediamo il tipo concreto di set allora.
-- set :: ReadWriteAttr Window String () -> String -> UI Window -> UI Window
-- questo e' quello che si intende con manipolazione dentro la monade UI

-- UI.addStyleSheet :: Window -> FilePath -> UI ()
-- questa funzione aggiunge il foglio di stile. L'unica cosa da notare qui
-- e' il fatto che il percorso non e' completo; la prima parte e' quella definita
-- nel campo tpStatic di Config. Di default e' Nothing (Maybe FilePath)

-- A questo punto, buttons e' [Element]
-- Cosa vuol dire Element? Element e' un dato senza costruttori, che si crea con
-- mkElement :: String -> UI Element, where String is the name tag of the element

-- getBody :: Window -> UI Element | prende il corpo della pagina, che poi e'
-- il foglio di stile, il fatto che abbia chiamato la scheda "Buttons"
-- e ci aggiunge un div :: UI Element

-- Come lo aggiunge:
-- (#+) :: UI Element -> [UI Element] -> UI Element
-- Il punto e' che li appende COME FIGLI dell'elemento principale

-- (#.) :: UI Element -> String -> UI Element
-- Questo e' un combinatore per settare la classe del css

-- notiamo che #+ ha fissita' 8 come #, e associano entrambi a sinistra

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Buttons"
    buttons <- mkButtons
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ map element buttons)]


-- Qui volevo solo una lista di elementi, e comincio con un titolo
-- quindi h1, dentro cui metto la stringa "Hello Haskell!", in un modo
-- analogo a <h1>Hello Haskell!<h1>. Notiamo che string :: String -> UI Element

greet :: [UI Element]
greet =
    [ UI.h1 #+ [string "Hello, Haskell!"]
    , UI.div #+ [string "Try the buttons below, they hover and click."]
    ]

-- Come si fa un bottone, e perche' ritorna due cose.
-- nella variabile button si mette un bottone taggato in html con "button"
-- e poi gli si mette come caption il valore desiderato. Questo perche'
-- stiamo parafrasando l'html <button type="button">Click Me!</button>
-- invece quello che lui chiama view non e' altro che un paragrafo che contiene
-- il bottone. Questo lo fa perche' in questo modo usa il bottone per deciderne
-- il comportamento, mentre tiene il view per stampare.

mkButton :: String -> UI (Element, Element)
mkButton caption = do
    button <- UI.button #. "button" #+ [string caption]
    view <- UI.p #+ [element button]
    return (button, view)

-- definisce una lista in cui ordinare gli elementi
-- crea il bottone e la sua view

-- on :: (element -> Event a) -> element -> (a -> UI void) -> UI ()
-- questa e' la prima funzione di interazione che stiamo osservando
-- e' una funzione di convenienza che ci consente di registrare eventi per elementi

-- per capire guardiamo il tipo
-- UI.click :: Element -> Event ()
-- ovvero click e' la funzione che dato un elemento ci restituisce un evento, concetto
-- di Reactive che quindi e' rappresenta uno stream di eventi,
-- semanticamente come type Event a = [(Time,a)]
-- Quindi se io indico un elemento mi voglio vedere tornare una lista temporale di certi valori
-- questo e' il punto; quando quella cosa succede, uso la funzione che usa quell'elemento
-- per effettuare una modifica all'interfaccia. Quei lambda appesi stanno a significare che
-- non me ne frega niente del valore che sto ottenendo, anche perche' si tratta solamente di segnali
-- che sono 1 o 0, e che quindi ritornano verosimilmente ().

-- altra funzione da capire qui e'
-- element :: (MonadIO m, Widget w) => w -> m Element
-- che e' una funzione di convenienza per return, da usare con set
-- text :: WriteAttr Element String

-- la domanda e': perche' una volta ha usato #+ per aggiungere dentro
-- al bottone una stringa, e la seconda ha settato con set text?


mkButtons :: UI [Element]
mkButtons = do
    list <- UI.ul #. "buttons-list"

    (button1, view1) <- mkButton button1Title

    on UI.hover button1 $ \_ -> element button1 # set text (button1Title ++ " [hover]")
    on UI.leave button1 $ \_ -> element button1 # set text button1Title
    on UI.click button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [pressed]")
        liftIO $ threadDelay 1000000
        element list #+ [UI.li # set html "<b>Delayed</b> result!"]

    (button2, view2) <- mkButton button2Title

    on UI.hover button2 $ \_ -> element button2 # set text (button2Title ++ " [hover]")
    on UI.leave button2 $ \_ -> element button2 # set text button2Title
    on UI.click button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [pressed]")
        element list #+ [UI.li # set html "Zap! Quick result!"]

    return [list, view1, view2]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"
