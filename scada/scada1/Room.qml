import QtQuick 2.0
import QtQuick.Layouts 1.11

Rectangle {
    border {
        color: "gray"
        width: 10
    }
    Layout.fillHeight: true
    Layout.fillWidth: true

    RowLayout {
        Image {
            Layout.margins: 10
            source: "bulb_off.png"
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
