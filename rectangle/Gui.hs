import           Foreign.Hoppy.Runtime (withScopedPtr)
import           Graphics.UI.Qtah.Core.QCoreApplication (exec)
import           Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QDoubleSpinBox as QDoubleSpinBox
import qualified Graphics.UI.Qtah.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment (getArgs)

main :: IO ()
main = withApp $ \_ -> do
    window <- makeWindow
    QWidget.show window
    exec
  where
    withApp = withScopedPtr $ getArgs >>= QApplication.new

data Input = A | B

makeWindow :: IO QWidget
makeWindow = do
    window <- QWidget.new

    -- input
    sideAInput <- QDoubleSpinBox.new
    sideBInput <- QDoubleSpinBox.new

    -- output
    perimeter <- QLabel.new
    diagonal  <- QLabel.new

    let update input x = do
            (a, b) <- case input of
                A -> do
                    b <- QDoubleSpinBox.value sideBInput
                    pure (x, b)
                B -> do
                    a <- QDoubleSpinBox.value sideAInput
                    pure (a, x)
            let p = 2 * (a + b)
            let d = sqrt (a * a + b * b)
            QLabel.setText perimeter $ show p
            QLabel.setText diagonal  $ show d

    update A 0
    connect_ sideAInput QDoubleSpinBox.valueChangedDoubleSignal $ update A
    connect_ sideBInput QDoubleSpinBox.valueChangedDoubleSignal $ update B

    do  layout <- QFormLayout.new
        QWidget.setLayout window layout

        QFormLayout.addRowStringWidget layout "Sides" sideAInput
        QFormLayout.addRowStringWidget layout "" sideBInput
        do  ruler <- QFrame.new
            QFrame.setFrameShape ruler QFrame.HLine
            QFormLayout.addRowWidget layout ruler
        QFormLayout.addRowStringWidget layout "Perimeter" perimeter
        QFormLayout.addRowStringWidget layout "Diagonal" diagonal

    pure window
