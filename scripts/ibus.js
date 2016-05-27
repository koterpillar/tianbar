/*
 * A plugin to show status of IBus.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  "use strict";

  var self = {};

  self.widget = function () {
    return $('.widget-ibus');
  };

  self.updated = $.Callbacks();

  // IBus DBus connection
  var bus;

  // Display the status of the input method
  function display() {
    var widget = self.widget();

    bus.call({
      'destination': 'org.freedesktop.IBus',
      'path': '/org/freedesktop/IBus',
      'iface': 'org.freedesktop.DBus.Properties',
      'member': 'Get',
      'body': [
        'string:org.freedesktop.IBus',
        'string:GlobalEngine'
      ]
    }).then(function (result) {
      var body = result.body[0];
      var imeName = body[3];
      var languageCode = body[5];
      var imeIcon = body[8];
      var imeSymbol = body[12];

      widget.empty();

      if (imeSymbol) {
        widget.text(imeSymbol);
      } else if (imeIcon) {
        if (!/\//.test(imeIcon)) {
          imeIcon = '/usr/share/icons/hicolor/scalable/apps/' + imeIcon + '.svg';
        }
        widget.append($('<img/>')
                      .attr('src', 'tianbar:///root/' + imeIcon)
                      .css({'height': '20px'}));
      } else {
        widget.text(languageCode);
      }

      widget
        .attr('title', imeName)
        .css({
          'display': 'inline-block',
          'vertical-align': 'middle',
          'width': '25px'
        });

      self.updated.fire();
    });
  }

  $(document).ready(function () {
    $.ajax('tianbar:///execute', {
      data: {
        command: 'ibus address'
      }
    }).then(function (address) {
      address = address.trim();
      dbus.connect(address).then(function (connectedBus) {
        bus = connectedBus;
        display();
        bus.listen({ member: 'GlobalEngineChanged' }).add(display);
      });
    });
  });

  return self;
});
