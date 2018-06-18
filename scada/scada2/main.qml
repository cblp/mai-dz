import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.11

ApplicationWindow {
    visible: true
    width: 800
    height: 600
    title: "SCADA 2"

    Room {
        id: roomA
        x: 100; y: 0; width: 700; height: 600
        Text {text: "A"; font.pixelSize: 50; x: 20; y: 530}
        Door {y: 410; width: 10; height: 100}
        EnterButton {x: -100; y: 400}
        ExitButton  {x:   10; y: 415; rotation: 180}
    }

    Room {
        id: roomB
        x: 450; y: 300; width: 335; height: 285
        Text {text: "B"; font.pixelSize: 50; x: 285; y: 215}
        Door {y: 110; width: 10; height: 100}
        EnterButton {x: -100; y: 100}
        ExitButton  {x:   10; y: 115; rotation: 180}
    }

    Room {
        id: roomC
        x: 115; y: 15; width: 670; height: 280
        Text {text: "C"; font.pixelSize: 50; x: 20; y: 10}
        Door {x: 115; y: 270; width: 100; height: 10}
        EnterButton {x: 110; y: 280; rotation: -90}
        ExitButton  {x: 125; y: 170; rotation:  90}
    }

    function enter(room) {
        if (room === roomA
            ? !room.entered
            : roomA.entered && !roomB.entered && !roomC.entered)
        {
            room.entered = true;
        }
    }

    function exit(room) {
        if (room === roomA
            ? roomA.entered && !roomB.entered && !roomC.entered
            : room.entered)
        {
            room.entered = false;
        }
    }
}
