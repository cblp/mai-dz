{-# LANGUAGE TypeApplications #-}

module MainWindow
    ( makeMainWindow
    ) where

import           Foreign.Hoppy.Runtime

import           QAbstractButton
import           QGridLayout
import           QMainWindow
import           QPushButton
import           Qt.Signal
import           QTabWidget
import           QWidget

import           DB
import           Tabs

makeMainWindow :: IO QWidget
makeMainWindow = do
    mainWindow <- QMainWindow.new
    setUnifiedTitleAndToolBarOnMac mainWindow True

    tabs <- QTabWidget.new
    setTabsClosable tabs True
    connect_        tabs tabCloseRequestedSignal $ closeTab tabs
    do
        (toolBarGrid, view) <- addQueryTab @Product tabs
                                                    "Продукция (Product)"
                                                    (Just "road-150")
                                                    []
        do
            displayWorkOrderButton <- QPushButton.newWithText
                "Заказы (WorkOrder)"
            connect_ displayWorkOrderButton QAbstractButton.clickedSignal
                $ \_ -> displayWorkOrder tabs view
            QGridLayout.addWidget toolBarGrid displayWorkOrderButton 0 1
        do
            displayBomButton <- QPushButton.newWithText
                "Компоненты (BillOfMaterials)"
            connect_ displayBomButton QAbstractButton.clickedSignal
                $ \_ -> displayBom tabs view
            QGridLayout.addWidget toolBarGrid displayBomButton 0 2
    setCentralWidget mainWindow tabs

    pure (QWidget.cast mainWindow)

closeTab :: QTabWidget -> Int -> IO ()
closeTab _    0 = pure ()
closeTab tabs i = delete =<< QTabWidget.widget tabs i
