/*
 * A plugin to show the power (e.g. battery) state through UPower.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  var deviceStatus = {};
  function updatePower() {
    dbus.system.call({
      'destination': 'org.freedesktop.UPower',
      'path': '/org/freedesktop/UPower',
      'iface': 'org.freedesktop.UPower',
      'member': 'EnumerateDevices',
      'body': [
      ]
    }, updateDeviceList);
  }

  function updateDeviceList(devices) {
    // TODO: promises in DBus instead of returning Right
    devices = devices.Right;
    if (!devices) {
      return;
    }
    devices = devices.body[0];
    // TODO: the whole thing should be on promises
    $.each(devices, function(_, device) {
      dbus.system.call({
        'destination': 'org.freedesktop.UPower',
        'path': device,
        'iface': 'org.freedesktop.DBus.Properties',
        'member': 'GetAll',
        'body': [
          'string:org.freedesktop.UPower.Device',
        ]
      }, function (result) { updateDevice(device, result); });
    });
  }

  function updateDevice(device, result) {
    result = result.Right;
    result = result.body[0];
    deviceStatus[device] = result;
    updateWidget();
  }

  function updateWidget() {
    var widget = $('.widget-power');
    widget.empty();
    // TODO: externalize styling
    $.each(deviceStatus, function (_, st) {
      // TODO: distinguish AC properly
      if (st.NativePath === 'AC') {
        return;
      }
      widget.append(st.Percentage);
    });
  }

  $(document).ready(function () {
    updatePower();
    setInterval(updatePower, 10000);
  });
});
