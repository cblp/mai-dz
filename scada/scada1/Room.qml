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

    GridLayout {
        flow: GridLayout.TopToBottom
        rows: 2

        property bool light: false

        Image {
            Layout.margins: 10
            Layout.preferredHeight: 129
            Layout.preferredWidth: 83
            source: parent.light ? "bulb_on.png" : "bulb_off.png"
        }

        Button {
            Layout.margins: 10
            text: "Свет"
            onClicked: parent.light = !parent.light;
        }

        Image {
            Layout.margins: 10
            source: "socket_off.png"
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
