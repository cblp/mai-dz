import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.11

ApplicationWindow {
    visible: true
    width: 800
    height: 600
    title: qsTr("SCADA 1")

    GridLayout {
        id: scene
        anchors.fill: parent
        columns: 2

        Room {
            id: room1
        }

        Room {
            id: room2
        }

        Room {
            id: room3
        }

        property var rooms: [room1, room2, room3];

        Item {
            Layout.fillHeight: true
            Layout.fillWidth: true

            ColumnLayout {
                Button {
                    text: "Выключить весь свет"
                    onClicked: {
                        for (var r in scene.rooms)
                            scene.rooms[r].light = false;
                    }
                }

                Button {
                    text: "Выключить всё электричество"
                    onClicked: {
                        for (var r in scene.rooms) {
                            scene.rooms[r].light = false;
                            scene.rooms[r].socket = false;
                        }
                    }
                }
            }
        }
    }
}
