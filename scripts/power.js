/*
 * A plugin to show the power (e.g. battery) state through UPower.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  function updatePower() {
    dbus.system.call({
      'destination': 'org.freedesktop.UPower',
      'path': '/org/freedesktop/UPower',
      'iface': 'org.freedesktop.UPower',
      'member': 'EnumerateDevices',
      'body': [
      ]
    }).done(function (devices) {
      devices = devices.body[0];
      var queries = $.map(devices, function(device) {
        return dbus.system.call({
          'destination': 'org.freedesktop.UPower',
          'path': device,
          'iface': 'org.freedesktop.DBus.Properties',
          'member': 'GetAll',
          'body': [
            'string:org.freedesktop.UPower.Device',
          ]
        });
      });
      $.when.apply($, queries).then(function (results) {
        results = Array.prototype.slice.call(arguments, 0);

        var widget = $('.widget-power');
        widget.empty();
        // TODO: externalize styling
        $.each(results, function (_, st) {
          st = st.body[0];
          // TODO: distinguish AC properly
          if (st.NativePath === 'AC') {
            return;
          }
          widget.append(st.Percentage);
        });
      });
    });
  }

  $(document).ready(function () {
    updatePower();
    setInterval(updatePower, 10000);
  });
});
