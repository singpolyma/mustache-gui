import Prelude hiding (lookup, mapM, mapM_)
import qualified Data.List (lookup)
import Data.Hashable
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk as Gtk
import Data.Tree
import Data.Traversable
import Data.Foldable
import Control.Applicative

type Address = [String]

data UpdateMessage =
	ReplaceMessage Address UpdateMessageItem |
	ListInsertMessage Address Int (Map String UpdateMessageItem) |
	ListRemoveMessage Address Int |
	ListUpdateMessage Address Int UpdateMessage

data UpdateMessageItem =
	StringItem String |
	ListItem [Map String UpdateMessageItem]

class Lookupable a where
	lookup :: (Ord k, Hashable k) => k -> a k v -> Maybe v

newtype AssocList k v = AssocList [(k,v)]

instance Lookupable AssocList where
	lookup k (AssocList xs) = Data.List.lookup k xs

class Updatable a where
	update :: a -> UpdateMessage -> IO a

type Context = [(String, String)] -- TODO

type Templatable a = (Context -> a)

data AttributeData = Grid {
		text :: Templatable String,
		rows :: Int,
		cols :: Int
	} |
	Button {
		_id :: String,
		text :: Templatable String
	}

data Widget a = Widget {
		setText :: (String -> IO ()),
		display :: IO (),
		unwrap :: a
	}

data GUI a = GUI Context (Tree (AttributeData, Widget a))

getElementById :: GUI a -> String -> Maybe (AttributeData,Widget a)
getElementById (GUI _ tree) k = find (match . fst) tree
	where
	match (Button {_id = s}) = s == k
	match _ = False

gtkWindowToWidget :: (Gtk.WindowClass o) => o -> Widget Gtk.Widget
gtkWindowToWidget w = Widget {
		setText = (\s -> Gtk.set w [ Gtk.windowTitle Gtk.:= s ]),
		display = Gtk.widgetShowAll w,
		unwrap = Gtk.castToWidget w
	}


gtkButtonToWidget :: (Gtk.ButtonClass o) => o -> Widget Gtk.Widget
gtkButtonToWidget w = Widget {
		setText = Gtk.buttonSetLabel w,
		display = Gtk.widgetShowAll w,
		unwrap = Gtk.castToWidget w
	}

nopWidget :: o -> Widget o
nopWidget w = Widget {
		setText = const $ return (),
		display = return (),
		unwrap = w
	}

-- Full-on bruteforce updates
instance Updatable (GUI a) where
	update (GUI ctx tree) (ReplaceMessage [a] (StringItem s)) =
		GUI ctx' <$> mapM updateWidget tree
		where
		updateWidget pair@(Grid {text = text}, w) =
			setText w (text ctx') >> return pair
		updateWidget pair@(Button {text = text}, w) =
			setText w (text ctx') >> return pair
		ctx' = (a,s) : ctx

aView = Node {
		rootLabel = Grid {
			text = const "Some Title",
			rows = 1,
			cols = 1
		},
		subForest = [
			Node {
				rootLabel = Button {
					_id = "toggler",
					text = fromMaybe "Nothing" . lookup "buttonLabel" . AssocList
				},
				subForest = []
			}
		]
	}

createGtkFromViewData :: Tree AttributeData -> IO (GUI Gtk.Widget)
createGtkFromViewData (Node {
                       rootLabel = g@(Grid {text=title, rows=rows, cols=cols}),
                       subForest = children}) = do
	toplevel <- Gtk.windowNew
	Gtk.set toplevel [ Gtk.windowTitle Gtk.:= title [] ] -- TODO

	toplevelTable <- fmap Gtk.castToWidget $ Gtk.tableNew rows cols False
	Gtk.containerAdd toplevel toplevelTable

	children' <- mapM (newChildOf toplevelTable) children

	return $ GUI [] $ Node {rootLabel = (g,gtkWindowToWidget toplevel), subForest = [
			Node {rootLabel = (g,nopWidget toplevelTable), subForest = children'}
		]}
	where
	newChildOf parent (Node {rootLabel = adata, subForest = children}) = do
		(gtk, me) <- single adata
		Gtk.containerAdd (Gtk.castToContainer parent) gtk
		children' <- mapM (newChildOf gtk) children
		return $ Node {rootLabel = (adata, me), subForest = children'}

	single (Grid {rows = rows, cols = cols}) = do
		t <- fmap Gtk.castToWidget $ Gtk.tableNew rows cols False
		return (t, nopWidget t)
	single (Button {text = text}) = do
		b <- Gtk.buttonNewWithLabel $ text [] -- TODO
		return (Gtk.castToWidget b, gtkButtonToWidget b)

main = do
	Gtk.initGUI
	gui@(GUI _ (Node {rootLabel = (_, window)})) <- createGtkFromViewData aView
	display window
	let Just (_,toggler) = gui `getElementById` "toggler"
	Gtk.on (Gtk.castToButton $ unwrap toggler) Gtk.buttonActivated $ do
		-- TODO: don't throw away the new GUI
		_ <- update gui (ReplaceMessage ["buttonLabel"] (StringItem "hello"))
		return ()
	Gtk.mainGUI
