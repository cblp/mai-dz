import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.11

Rectangle {
    id: room

    property bool entered: false

    border {
        color: entered ? "green" : "red"
        width: 10
    }
    Layout.fillHeight: true
    Layout.fillWidth: true

}
