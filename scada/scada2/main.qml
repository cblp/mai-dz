import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.11

ApplicationWindow {
    visible: true
    width: 800
    height: 600
    title: qsTr("SCADA 2")


    // Room A //////////////////////////////////////////////////////////////////

    Room {
        id: roomA
        x: 100; y: 0; width: 700; height: 600
        Text {text: "A"; font.pixelSize: 50; x: 20; y: 530}
    }

    ArrowButton {
        color: "green"
        x: 0; y: 400
        onClicked: tryEnterA()
    }

    ArrowButton {
        color: "red"
        x: 110; y: 415
        rotation: 180
        onClicked: tryExitA()
    }


    // Room B //////////////////////////////////////////////////////////////////

    Room {
        id: roomB
        x: 450; y: 300; width: 335; height: 285
        Text {text: "B"; font.pixelSize: 50; x: 285; y: 215}
    }

    ArrowButton {
        color: "green"
        x: 350; y: 400
        onClicked: tryEnterB()
    }

    ArrowButton {
        color: "red"
        x: 460; y: 415
        rotation: 180
        onClicked: tryExitB()
    }


    // Room C //////////////////////////////////////////////////////////////////

    Room {
        id: roomC
        x: 115; y: 15; width: 670; height: 280
        Text {text: "C"; font.pixelSize: 50; x: 20; y: 10}
    }

    ArrowButton {
        color: "green"
        x: 220; y: 295
        rotation: -90
        onClicked: tryEnterC()
    }

    ArrowButton {
        color: "red"
        x: 235; y: 185
        rotation: 90
        onClicked: tryExitC()
    }


    // Movement Resrtictions ///////////////////////////////////////////////////

    function tryEnterA() {
        if (!roomA.entered)
            roomA.entered = true;
    }

    function tryExitA() {
        if (roomA.entered && !roomB.entered && !roomC.entered)
            roomA.entered = false;
    }

    function tryEnterB() {
        if (roomA.entered && !roomB.entered && !roomC.entered)
            roomB.entered = true;
    }

    function tryExitB() {
        if (roomB.entered)
            roomB.entered = false;
    }

    function tryEnterC() {
        if (roomA.entered && !roomB.entered && !roomC.entered)
            roomC.entered = true;
    }

    function tryExitC() {
        if (roomC.entered)
            roomC.entered = false;
    }
}
