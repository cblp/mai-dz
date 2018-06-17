import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.11

ApplicationWindow {
    visible: true
    width: 800
    height: 600
    title: qsTr("SCADA 1")

    GridLayout {
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

        Item {
            Layout.fillHeight: true
            Layout.fillWidth: true

            ColumnLayout {
                Button {
                    text: "Выключить весь свет"
                    onClicked: {
                        room1.light = false;
                        room2.light = false;
                        room3.light = false;
                    }
                }

                Button {
                    text: "Выключить всё электричество"
                    onClicked: {
                        room1.light = false;
                        room2.light = false;
                        room3.light = false;
                        room1.socket = false;
                        room2.socket = false;
                        room3.socket = false;
                    }
                }
            }
        }
    }
}
