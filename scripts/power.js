/*
 * A plugin to show the power (e.g. battery) state through UPower.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  var self = {
    /**
     * Format an HTML element displaying the device status.
     * @param st {Object} The device status, as returned by UPower
     */
    formatDevice: function (st) {
      // TODO: better default formatting
      return st.Percentage;
    }
  };

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
          // Do not show AC power
          if (st.Type === 1) {
            return;
          }
          widget.append(self.formatDevice(st));
        });
      });
    });
  }

  $(document).ready(function () {
    updatePower();
    setInterval(updatePower, 10000);
  });

  return self;
});