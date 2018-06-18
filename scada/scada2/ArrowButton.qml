import QtQuick 2.9

MouseArea {
    property string color: "black"
    width: 100
    height: 100

    Text {
        font.pixelSize: 100
        text: "➡"
        color: parent.color
    }
}
