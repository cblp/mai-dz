import           Foreign.Hoppy.Runtime
import           System.Environment

import           QApplication
import           QCoreApplication
import           QWidget

import           MainWindow

main :: IO ()
main = withApp $ \_ -> do
    mainWindow <- makeMainWindow
    showMaximized mainWindow
    exec
  where
    withApp = withScopedPtr $ do
        args <- getArgs
        QApplication.new args
