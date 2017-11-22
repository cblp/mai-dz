-- http://khumba.net/docs/qtah-qt5-0.3.0

import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import           Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QFrame as QFrame
import           Graphics.UI.Qtah.Widgets.QLabel as QLabel
import           Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) mainApp

mainApp :: QApplication -> IO ()
mainApp _ = do
    hello <- newWithText "hello"
    setFrameShape hello StyledPanel
    QWidget.show hello
    exec
