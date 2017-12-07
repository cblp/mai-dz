-- http://khumba.net/docs/qtah-qt5-0.3.0

import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import           Graphics.UI.Qtah.Core.QSize as QSize
import           Graphics.UI.Qtah.Gui.QIcon as QIcon
import           Graphics.UI.Qtah.Widgets.QAbstractButton (setIconSize)
import           Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import           Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLayout as QLayout
import           Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import           Graphics.UI.Qtah.Widgets.QTreeView as QTreeView
import           Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget, setLayout)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

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
    fileImage <- QIcon.newWithFile "icons/fileImage.png"
    fileList <- QIcon.newWithFile "icons/fileList.png"
    laptop <- QIcon.newWithFile "icons/laptop.png"

    buttonIconSize <- QSize.new 32 32

    appWindow <- QWidget.new
    mainLayout <- QHBoxLayout.new
    setLayout appWindow mainLayout

    let addButton icon text layout = do
            button <-
                QPushButton.newWithIconAndTextAndParent icon text appWindow
            setIconSize button buttonIconSize
            QBoxLayout.addWidget layout button

    do  leftPanel <- QVBoxLayout.new
        addItem mainLayout leftPanel

        _addVerticalCablingButton <-
            addButton connectionV "Добавить\nвертикальную подсистему" leftPanel
        _addHorizontalCablingButton <-
            addButton
                connectionH "Добавить\nгоризонтальную подсистему" leftPanel
        _addWorkPlaceButton <-
            addButton laptop "Добавить\nрабочее место" leftPanel
        addStretch leftPanel

    do  workArea <- QTreeWidget.newWithParent appWindow
        setHeaderHidden workArea True
        QBoxLayout.addWidget mainLayout workArea

    do  rightPanel <- QVBoxLayout.new
        addItem mainLayout rightPanel

        addStretch rightPanel
        _saveTextButton <-
            addButton fileList "Сохранить\nтекстовое описание" rightPanel
        _saveImageButton <-
            addButton fileImage "Сохранить\nизображение" rightPanel
        pure ()

    pure appWindow
