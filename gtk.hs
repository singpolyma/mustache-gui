import Prelude hiding (lookup, mapM, mapM_)
import Control.Monad hiding (mapM, mapM_)
import qualified Data.List (lookup)
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk as Gtk
import Data.Tree
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Data.IORef

type Address = [String]

data UpdateMessage =
	ReplaceMessage Address ContextItem |
	ListInsertMessage Address Int Context |
	ListRemoveMessage Address Int |
	ListUpdateMessage Address Int UpdateMessage

class Lookupable a where
	lookup :: (Ord k, Hashable k) => k -> a k v -> Maybe v

newtype AssocList k v = AssocList [(k,v)]

instance Lookupable AssocList where
	lookup k (AssocList xs) = Data.List.lookup k xs

instance Lookupable Map where
	lookup = Map.lookup

class Updatable a where
	update :: a -> UpdateMessage -> IO a

data ContextItem =
	StringCtx String |
	ListCtx [Context]
type Context = Map String ContextItem

instance Show ContextItem where
	show (StringCtx s) = s
	show (ListCtx xs) = show xs

type Templatable a = (Context -> a)

data AttributeData = Grid {
		text :: Templatable String,
		rows :: Int,
		cols :: Int
	} |
	Button {
		_id :: String,
		text :: Templatable String
	} |
	Label {
		text :: Templatable String
	} |
	Section {
		variable :: String,
		chunk :: [Tree AttributeData]
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

gtkLabelToWidget :: (Gtk.LabelClass o) => o -> Widget Gtk.Widget
gtkLabelToWidget w = Widget {
		setText = Gtk.labelSetText w,
		display = Gtk.widgetShowAll w,
		unwrap = Gtk.castToWidget w
	}

nopWidget :: o -> Widget o
nopWidget w = Widget {
		setText = const $ return (),
		display = return (),
		unwrap = w
	}

listInsertAt :: Int -> a -> [a] -> [a]
listInsertAt idx v xs = before ++ v:after
	where
	(before, after) = splitAt idx xs

-- Full-on bruteforce updates
instance Updatable (GUI a) where
	update (GUI ctx tree) (ListInsertMessage [a] idx item) =
		GUI ctx' <$> mapM updateWidget tree
		where
		updateWidget pair@(Section {variable = v, chunk = c}, w) = do
			-- TODO
			print $ "Section for " ++ v ++ "(" ++ show (lookup v ctx') ++ ")"
			return pair
		updateWidget pair = return pair
		ctx' = Map.alter (\mxs ->
				Just $ case mxs of
					Just (ListCtx xs) -> ListCtx $ listInsertAt idx item xs
					_ -> ListCtx [item]
			) a ctx

	update (GUI ctx tree) (ReplaceMessage [a] item) =
		GUI ctx' <$> mapM updateWidget tree
		where
		updateWidget pair@(Grid {text = text}, w) =
			setText w (text ctx') >> return pair
		updateWidget pair@(Button {text = text}, w) =
			setText w (text ctx') >> return pair
		updateWidget pair@(Label {text = text}, w) =
			setText w (text ctx') >> return pair
		updateWidget pair@(Section {variable = v, chunk = c}, w) = do
			-- TODO
			print $ "Section for " ++ v ++ "(" ++ show (lookup v ctx') ++ ")"
			return pair
		ctx' = Map.insert a item ctx

aView = Node {
		rootLabel = Grid {
			text = const "Test App",
			rows = 2,
			cols = 1
		},
		subForest = [
			Node {
				rootLabel = Button {
					_id = "toggler",
					text = show . fromMaybe (StringCtx "") . lookup "togglerLabel"
				},
				subForest = []
			},
			Node {
				rootLabel = Button {
					_id = "adder",
					text = const "Add item!"
				},
				subForest = []
			},
			Node {
				rootLabel = Grid {
					text = const "",
					rows = 3,
					cols = 1
				},
				subForest = [
					Node {
						rootLabel = Label {
							text = const "outer"
						},
						subForest = []
					},
					Node {
						rootLabel = Section {
							variable = "items",
							chunk = error "No chunk on raw parsed AttributeData"
						},
						subForest = [
							Node {
								rootLabel = Label {
									text = (\ctx -> "hello " ++ (show $ fromMaybe (StringCtx "") $ lookup "text" ctx) ++ "man")
								},
								subForest = []
							},
							Node {
								rootLabel = Button {
									_id = "",
									text = const "button text"
								},
								subForest = []
							}
						]
					}
				]
			}
		]
	}

createGtkFromViewData :: Tree AttributeData -> IO (GUI Gtk.Widget)
createGtkFromViewData (Node {
                       rootLabel = g@(Grid {text=title, rows=rows, cols=cols}),
                       subForest = children}) = do
	toplevel <- Gtk.windowNew
	Gtk.set toplevel [ Gtk.windowTitle Gtk.:= title mempty ] -- TODO

	toplevelTable <- fmap Gtk.castToWidget $ Gtk.tableNew rows cols False
	Gtk.containerAdd toplevel toplevelTable

	children' <- mapM (newChildOf toplevelTable) children

	return $ GUI mempty $ Node {rootLabel = (g,gtkWindowToWidget toplevel), subForest = [
			Node {rootLabel = (g,nopWidget toplevelTable), subForest = children'}
		]}
	where
	newChildOf parent (Node {rootLabel = adata, subForest = children}) = do
		s <- single adata
		case s of
			Just (gtk, me) -> do
				-- Currently acting like VBox. Need better layout algorithm
				y <- length <$> Gtk.containerGetChildren (Gtk.castToContainer parent)
				Gtk.tableAttachDefaults (Gtk.castToTable parent) gtk 0 1 y (y+1)
				children' <- mapM (newChildOf gtk) children
				return $ Node {rootLabel = (adata, me), subForest = children'}
			_ -> do
				let adata' = adata {chunk = children}
				return $ Node {rootLabel = (adata', nopWidget (error "cannot unwrap pseudo-widget")), subForest = []}

	single (Grid {rows = rows, cols = cols}) = do
		t <- fmap Gtk.castToWidget $ Gtk.tableNew rows cols False
		return $ Just (t, nopWidget t)
	single (Button {_id = _id, text = text}) = do
		b <- Gtk.buttonNewWithLabel $ text mempty -- TODO
		Gtk.widgetSetName b _id
		return $ Just (Gtk.castToWidget b, gtkButtonToWidget b)
	single (Label {text = text}) = do
		l <- Gtk.labelNew $ Just $ text mempty -- TODO
		return $ Just (Gtk.castToWidget l, gtkLabelToWidget l)
	single (Section {}) = return Nothing

main = do
	Gtk.initGUI
	gui@(GUI _ (Node {rootLabel = (_, window)})) <- createGtkFromViewData aView
	display window

	let Just (_,toggler) = gui `getElementById` "toggler"
	guiRef <- newIORef gui
	Gtk.on (Gtk.castToButton $ unwrap toggler) Gtk.buttonActivated $ do
		gui@(GUI ctx _) <- readIORef guiRef
		let txt = case lookup "togglerLabel" ctx of
			Just (StringCtx "on") -> "off"
			_ -> "on"
		writeIORef guiRef =<< update gui (ReplaceMessage ["togglerLabel"] (StringCtx txt))

	let Just (_,adder) = gui `getElementById` "adder"
	Gtk.on (Gtk.castToButton $ unwrap adder) Gtk.buttonActivated $ do
		gui@(GUI ctx _) <- readIORef guiRef
		writeIORef guiRef =<< update gui (ListInsertMessage ["items"] 0 (Map.fromList [("text",StringCtx "new")]))

	Gtk.on (unwrap window) Gtk.unrealize $ do
		Gtk.mainQuit
	Gtk.mainGUI
