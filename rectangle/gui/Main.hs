import           Foreign.Hoppy.Runtime
import           Graphics.UI.Qtah.Core.QCoreApplication
import           Graphics.UI.Qtah.Signal
import           Graphics.UI.Qtah.Widgets.QApplication as QApplication
import           Graphics.UI.Qtah.Widgets.QDoubleSpinBox as QDoubleSpinBox
import           Graphics.UI.Qtah.Widgets.QFormLayout as QFormLayout
import           Graphics.UI.Qtah.Widgets.QFrame as QFrame
import           Graphics.UI.Qtah.Widgets.QLabel as QLabel
import           Graphics.UI.Qtah.Widgets.QWidget (QWidget)
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import           System.Environment

main :: IO ()
main = withApp $ \_ -> do
    window <- makeWindow
    QWidget.show window
    exec
  where
    withApp = withScopedPtr (getArgs >>= QApplication.new)

makeWindow :: IO QWidget
makeWindow = do
    window <- QWidget.new

    -- input
    sideA <- newSideInput
    sideB <- newSideInput

    -- output
    perimeter <- QLabel.new
    diagonal  <- QLabel.new

    let update theOtherSide x = do
            y <- value theOtherSide
            let p = 2 * (x + y)
            let d = sqrt (x * x + y * y)
            setText perimeter (show p)
            setText diagonal  (show d)

    update sideA 0
    connect_ sideA valueChangedDoubleSignal (update sideB)
    connect_ sideB valueChangedDoubleSignal (update sideA)

    do  form <- QFormLayout.new
        QWidget.setLayout window form
        addRowStringWidget form "Side A" sideA
        addRowStringWidget form "Side B" sideB
        addRowWidget form =<< QFrame.new .+ (`setFrameShape` QFrame.HLine)
        addRowStringWidget form "Perimeter" perimeter
        addRowStringWidget form "Diagonal"  diagonal

    pure window

  where
    newSideInput = QDoubleSpinBox.new .+ (`setMaximum` (10 ^ (300 :: Int)))

(.+) :: Monad m => m a -> (a -> m ()) -> m a
create .+ configure = do
    a <- create
    configure a
    pure a
