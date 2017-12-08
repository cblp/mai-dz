import           Control.Monad (unless, void)
import           Data.Foldable (fold)
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Monoid (Sum (Sum))
import           Data.Traversable (for)
import           Foreign.Hoppy.Runtime (nullptr, withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import qualified Graphics.UI.Qtah.Core.QSize as QSize
import           Graphics.UI.Qtah.Gui.QIcon (QIcon)
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import           Graphics.UI.Qtah.Gui.QPixmap (save)
import           Graphics.UI.Qtah.Signal (connect_)
import           Graphics.UI.Qtah.Widgets.QAbstractButton (clickedSignal,
                                                           setIconSize)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QBoxLayout (QBoxLayoutPtr, addStretch,
                                                      addWidget)
import qualified Graphics.UI.Qtah.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLayout (addItem)
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import           Graphics.UI.Qtah.Widgets.QTreeView (setHeaderHidden)
import           Graphics.UI.Qtah.Widgets.QTreeWidget (currentItem,
                                                       setCurrentItem,
                                                       topLevelItem,
                                                       topLevelItemCount)
import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QTreeWidgetItem (child, childCount,
                                                           getType, parent,
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
    mainWindow <- makeMainWindow
    QWidget.show mainWindow
    exec
  where
    withApp = withScopedPtr $ getArgs >>= QApplication.new

makeMainWindow :: IO QWidget
makeMainWindow = do
    connectionH <- QIcon.newWithFile "icons/connectionH.png"
    connectionV <- QIcon.newWithFile "icons/connectionV.png"
    fileImage   <- QIcon.newWithFile "icons/fileImage.png"
    fileList    <- QIcon.newWithFile "icons/fileList.png"
    laptop      <- QIcon.newWithFile "icons/laptop.png"

    buttonIconSize <- QSize.new 32 32

    counter <- newIORef (0 :: Natural)

    mainWindow <- QWidget.new
    mainLayout <- QHBoxLayout.new
    setLayout mainWindow mainLayout

    workArea <- QTreeWidget.newWithParent mainWindow
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
                    QMessageBox.critical
                        mainWindow
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
                    QMessageBox.critical
                        mainWindow
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

    let saveText :: IO ()
        saveText = do
            fileName <-
                QFileDialog.getSaveFileName
                    mainWindow
                    "Сохранить текстовое описание"
                    "cabling.txt"
                    "Text files (*.txt)"
            unless (null fileName) $ do
                cablingVCount <- topLevelItemCount workArea
                (Sum cablingHCount, Sum workPlaceCount) <-
                    fmap fold $ for [0 .. cablingVCount - 1] $ \i -> do
                        cablingV <- topLevelItem workArea i
                        cablingHCount <- childCount cablingV
                        workPlaceCount <-
                            fmap fold $ for [0 .. cablingHCount - 1] $ \j -> do
                                cablingH <- child cablingV j
                                Sum <$> childCount cablingH
                        pure (Sum cablingHCount, workPlaceCount)
                writeFile fileName $
                    unlines
                        [ "Вертикальных систем: " ++ show cablingVCount
                        , "Горизонтальных систем: " ++ show cablingHCount
                        , "Рабочих мест: " ++ show workPlaceCount
                        ]

    let saveImage :: IO ()
        saveImage = do
            fileName <-
                QFileDialog.getSaveFileName
                    mainWindow
                    "Сохранить изображение"
                    "cabling.png"
                    "Images (*.png)"
            unless (null fileName) $ do
                shot <- QWidget.grab workArea
                ok <- save shot fileName
                unless ok $ void $
                    QMessageBox.critical
                        mainWindow
                        "Не удалось сохранить файл"
                        "Не удалось сохранить изображение. Возможно, вы неверно указали расширение. Попробуйте png."

    let addButton ::
            QBoxLayoutPtr layout => QIcon -> String -> layout -> IO () -> IO ()
        addButton icon text layout handler = do
            button <-
                QPushButton.newWithIconAndTextAndParent icon text mainWindow
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
        addButton fileList "Сохранить\nтекстовое описание" rightPanel saveText
        addButton fileImage "Сохранить\nизображение" rightPanel saveImage

    -- test
    QWidget.show mainWindow
    addCablingV
    addCablingH
    addWorkPlace
    addCablingV
    addCablingV
    addCablingH
    addCablingH
    addWorkPlace
    addWorkPlace
    addWorkPlace
    addWorkPlace
    saveImage
    -- end test

    pure mainWindow

preIncrement :: Enum a => IORef a -> IO a
preIncrement ref = do
    modifyIORef ref succ
    readIORef ref
