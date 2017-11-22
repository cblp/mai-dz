-- http://khumba.net/docs/qtah-qt5-0.3.0

import           Foreign.Hoppy.Runtime                  (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QApplication  as QApplication
import qualified Graphics.UI.Qtah.Widgets.QLabel        as QLabel
import qualified Graphics.UI.Qtah.Widgets.QWidget       as QWidget
import           System.Environment                     (getArgs)

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) mainUi

mainUi :: t -> IO ()
mainUi _ = do
    do  hello <- QLabel.newWithText "hello"
        QLabel.setMargin hello 20
        QWidget.show hello
    QWidget.show =<< QLabel.newWithText "world"
    QCoreApplication.exec
