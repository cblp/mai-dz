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
        }

        Room {
        }

        Room {
        }
    }
}
