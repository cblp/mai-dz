import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import           Graphics.UI.Qtah.Signal (connect_)
import           Graphics.UI.Qtah.Widgets.QAbstractButton (pressedSignal,
                                                           setIconSize)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QBoxLayout (addStretch, addWidget)
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLayout (addItem)
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (addTopLevelItem)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (setIcon, setText)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget, setLayout)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           Numeric.Natural (Natural)
import           System.Environment (getArgs)

data ItemType = VerticalCabling | HorizontalCabling | WorkPlace
    deriving (Enum)

main :: IO ()
main = withApp $ \_ -> do
    appWindow <- makeAppWindow
    QWidget.show appWindow
    exec
  where
    withApp = withScopedPtr $ getArgs >>= QApplication.new

makeAppWindow :: IO QWidget
makeAppWindow = do
    connectionH <- QIcon.newWithFile "icons/connectionH.png"
    connectionV <- QIcon.newWithFile "icons/connectionV.png"
    fileImage   <- QIcon.newWithFile "icons/fileImage.png"
    fileList    <- QIcon.newWithFile "icons/fileList.png"
    laptop      <- QIcon.newWithFile "icons/laptop.png"

    buttonIconSize <- QSize.new 32 32

    counter <- newIORef (0 :: Natural)

    appWindow <- QWidget.new
    mainLayout <- QHBoxLayout.new
    setLayout appWindow mainLayout

    workArea <- QTreeWidget.newWithParent appWindow
    setHeaderHidden workArea True

    let addVerticalCabling = do
            n <- preIncrement counter
            item <- QTreeWidgetItem.newWithType $ fromEnum VerticalCabling
            setIcon item 0 connectionV
            setText item 0 $ "Вертикальная подсистема " ++ show n
            addTopLevelItem workArea item

    let addButton icon text layout handler = do
            button <-
                QPushButton.newWithIconAndTextAndParent icon text appWindow
            setIconSize button buttonIconSize
            addWidget layout button
            connect_ button pressedSignal handler

    do  leftPanel <- QVBoxLayout.new
        addItem mainLayout leftPanel

        addButton
            connectionV
            "Добавить\nвертикальную подсистему"
            leftPanel
            addVerticalCabling
        addButton connectionH "Добавить\nгоризонтальную подсистему" leftPanel $
            pure ()
        addButton laptop "Добавить\nрабочее место" leftPanel $ pure ()
        addStretch leftPanel

    addWidget mainLayout workArea

    do  rightPanel <- QVBoxLayout.new
        addItem mainLayout rightPanel

        addStretch rightPanel
        addButton fileList "Сохранить\nтекстовое описание" rightPanel $ pure ()
        addButton fileImage "Сохранить\nизображение" rightPanel $ pure ()

    pure appWindow

preIncrement :: Enum a => IORef a -> IO a
preIncrement ref = do
    modifyIORef ref succ
    readIORef ref
