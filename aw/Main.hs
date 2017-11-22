import           Foreign.Hoppy.Runtime                  (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QApplication  as QApplication
import qualified Graphics.UI.Qtah.Widgets.QLabel        as QLabel
import qualified Graphics.UI.Qtah.Widgets.QWidget       as QWidget
import           System.Environment                     (getArgs)

main :: IO ()
main =
    withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
        lbl <- QLabel.new
        QWidget.show lbl
        QCoreApplication.exec
