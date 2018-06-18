import QtQuick 2.9
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3

Rectangle {
    id: room

    border {
        color: "gray"
        width: 10
    }
    Layout.fillHeight: true
    Layout.fillWidth: true

    property bool light: false
    property bool socket: true
    property bool motion: false

    GridLayout {
        flow: GridLayout.TopToBottom
        rows: 2

        Image {
            Layout.margins: 10
            Layout.preferredHeight: 129
            Layout.preferredWidth: 83
            source: room.light || room.motion ? "bulb_on.png" : "bulb_off.png"
        }

        Button {
            Layout.margins: 10
            text: "Свет"
            onClicked: room.light = !room.light;
        }

        Image {
            Layout.margins: 10
            source: room.socket ? "socket_on.png" : "socket_off.png"
        }

        Button {
            Layout.margins: 10
            text: "Розетка"
            onClicked: room.socket = !room.socket;
        }

        MouseArea {
            height: 100
            width: 100
            hoverEnabled: true

            Text {
                text: room.motion ? "🔴" : "⚫️"
                x: 40
                y: 30
            }

            Timer {
                interval: 200
                running: true
                repeat: true

                property int timeout: 0

                onTriggered: {
                    if (parent.containsMouse) {
                        timeout = 10;
                        room.motion = true;
                    } else {
                        if (timeout > 0)
                            --timeout;
                        else
                            room.motion = false;
                    }
                }
            }
        }
    }
}
