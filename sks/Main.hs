import           Control.Monad (void)
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Foreign.Hoppy.Runtime (nullptr, withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Core.QSize as QSize
import           Graphics.UI.Qtah.Gui.QIcon (QIcon)
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import           Graphics.UI.Qtah.Signal (connect_)
import           Graphics.UI.Qtah.Widgets.QAbstractButton (clickedSignal,
                                                           setIconSize)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QBoxLayout (QBoxLayoutPtr, addStretch,
                                                      addWidget)
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLayout (addItem)
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (currentItem,
                                                       setCurrentItem)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (getType, parent,
                                                           setIcon)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidgetItem as QTreeWidgetItem
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget, setLayout)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           Numeric.Natural (Natural)
import           System.Environment (getArgs)

data ItemType = CablingV | CablingH | WorkPlace
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

    let addCablingV :: IO ()
        addCablingV = do
            n <- preIncrement counter
            item <-
                QTreeWidgetItem.newWithParentTreeAndStringsAndType
                    workArea
                    ["Вертикальная подсистема " ++ show n]
                    (fromEnum CablingV)
            setIcon item 0 connectionV
            setCurrentItem workArea item

    let addCablingH :: IO ()
        addCablingH = do
            curItem <- currentItem workArea
            curItemType <-
                if curItem /= nullptr then
                    Just . toEnum <$> getType curItem
                else
                    pure Nothing
            case curItemType of
                Just CablingV -> addCablingH' curItem
                Just CablingH -> addCablingH' =<< parent curItem
                _ -> void $
                    QMessageBox.information
                        appWindow
                        "Не выбрана вертикальная подсистема"
                        "Выберите вертикальную подсистему, чтобы добавить к ней горизонтальную."
        addCablingH' curItem = do
            n <- preIncrement counter
            item <-
                QTreeWidgetItem.newWithParentItemAndStringsAndType
                    curItem
                    ["Горизонтальная подсистема " ++ show n]
                    (fromEnum CablingH)
            setIcon item 0 connectionH
            setCurrentItem workArea item

    let addWorkPlace :: IO ()
        addWorkPlace = do
            curItem <- currentItem workArea
            curItemType <-
                if curItem /= nullptr then
                    Just . toEnum <$> getType curItem
                else
                    pure Nothing
            case curItemType of
                Just CablingH  -> addWorkPlace' curItem
                Just WorkPlace -> addWorkPlace' =<< parent curItem
                _ -> void $
                    QMessageBox.information
                        appWindow
                        "Не выбрана горизонтальная подсистема"
                        "Выберите горизонтальную подсистему, чтобы добавить к ней рабочее место."
        addWorkPlace' curItem = do
            n <- preIncrement counter
            item <-
                QTreeWidgetItem.newWithParentItemAndStringsAndType
                    curItem
                    ["Рабочее место " ++ show n]
                    (fromEnum WorkPlace)
            setIcon item 0 laptop
            setCurrentItem workArea item

    let addButton ::
            QBoxLayoutPtr layout => QIcon -> String -> layout -> IO () -> IO ()
        addButton icon text layout handler = do
            button <-
                QPushButton.newWithIconAndTextAndParent icon text appWindow
            setIconSize button buttonIconSize
            addWidget layout button
            connect_ button clickedSignal $ const handler

    do  leftPanel <- QVBoxLayout.new
        addItem mainLayout leftPanel

        addButton
            connectionV
            "Добавить\nвертикальную подсистему"
            leftPanel
            addCablingV
        addButton
            connectionH
            "Добавить\nгоризонтальную подсистему"
            leftPanel
            addCablingH
        addButton laptop "Добавить\nрабочее место" leftPanel addWorkPlace
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
