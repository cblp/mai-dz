import QtQuick 2.9
import QtQuick.Layouts 1.3

Rectangle {
    id: room

    property bool entered: false
    property string text: ""

    border {
        color: entered ? "green" : "red"
        width: 10
    }
    Layout.fillHeight: true
    Layout.fillWidth: true

    Text {
        anchors.bottomMargin: 10
        anchors.fill: parent
        anchors.leftMargin: 20
        font.pixelSize: 50
        text: parent.text
        verticalAlignment: Text.AlignBottom
    }
}
