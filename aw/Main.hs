import           Foreign.Hoppy.Runtime                  (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import           Graphics.UI.Qtah.Widgets.QApplication  as QApplication
import           Graphics.UI.Qtah.Widgets.QLabel        as QLabel
import           Graphics.UI.Qtah.Widgets.QWidget       as QWidget
import           System.Environment                     (getArgs)

main :: IO ()
main =
    withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
        lbl <- QLabel.new
        QWidget.show lbl
        QCoreApplication.exec
