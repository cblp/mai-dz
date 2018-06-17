import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.11

Rectangle {
    border {
        color: "gray"
        width: 10
    }
    Layout.fillHeight: true
    Layout.fillWidth: true

    property bool light: false
    property bool socket: true

    GridLayout {
        flow: GridLayout.TopToBottom
        rows: 2

        Image {
            Layout.margins: 10
            Layout.preferredHeight: 129
            Layout.preferredWidth: 83
            source: parent.parent.light ? "bulb_on.png" : "bulb_off.png"
        }

        Button {
            Layout.margins: 10
            text: "Свет"
            onClicked: parent.parent.light = !parent.parent.light;
        }

        Image {
            Layout.margins: 10
            source: parent.parent.socket ? "socket_on.png" : "socket_off.png"
        }

        Button {
            Layout.margins: 10
            text: "Розетка"
            onClicked: parent.parent.socket = !parent.parent.socket;
        }

        Rectangle {
            border.color: "black"
            height: 100
            width: 100

            Text {
                text: "⚫️" // 🔴
                x: 40
                y: 30
            }
        }
    }
}
