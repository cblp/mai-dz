{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Foldable (for_)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (QTreeWidget)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

import           DB (Entity, Product, runDB, selectList)

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeProductView
    QWidget.showMaximized mainWindow
    exec
    where withApp = withScopedPtr $ getArgs >>= QApplication.new

makeProductView :: IO QTreeWidget
makeProductView = do
    productView <- QTreeWidget.new
    setHeaderHidden productView True

    products :: [Entity Product] <- runDB $ selectList [] []
    for_ products print

    pure productView
