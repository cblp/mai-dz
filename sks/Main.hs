-- http://khumba.net/docs/qtah-qt5-0.3.0

import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import           Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import           Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import           Graphics.UI.Qtah.Widgets.QLayout as QLayout
import           Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import           Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget
import           Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import           Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

main :: IO ()
main = withApp $ \_ -> do
    appWindow <- QWidget.new
    mainLayout <- QHBoxLayout.new
    setLayout appWindow mainLayout

    do  leftPanel <- QVBoxLayout.new
        addItem mainLayout leftPanel

        do  addVerticalCablingButton <-
                QPushButton.newWithTextAndParent
                    "Добавить\nвертикальную подсистему"
                    appWindow
            QBoxLayout.addWidget leftPanel addVerticalCablingButton

        do  addHorizontalCablingButton <-
                QPushButton.newWithTextAndParent
                    "Добавить\nгоризонтальную подсистему"
                    appWindow
            QBoxLayout.addWidget leftPanel addHorizontalCablingButton

        do  addWorkPlaceButton <-
                QPushButton.newWithTextAndParent
                    "Добавить\nрабочее место"
                    appWindow
            QBoxLayout.addWidget leftPanel addWorkPlaceButton

        addStretch leftPanel

    do  workArea <- QTreeWidget.new
        QBoxLayout.addWidget mainLayout workArea

    do  rightPanel <- QVBoxLayout.new
        addItem mainLayout rightPanel

        addStretch rightPanel

        do  saveTextButton <-
                QPushButton.newWithTextAndParent
                    "Сохранить\nтекстовое описание"
                    appWindow
            QBoxLayout.addWidget rightPanel saveTextButton

        do  saveImageButton <-
                QPushButton.newWithTextAndParent
                    "Сохранить\nизображение"
                    appWindow
            QBoxLayout.addWidget rightPanel saveImageButton

    QWidget.show appWindow
    exec
  where
    withApp = withScopedPtr $ getArgs >>= QApplication.new
