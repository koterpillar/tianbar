/*
 * A plugin to show status of IBus.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './command', './dbus'], function ($, command, dbus) {
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

    bus.getProperty(
      'org.freedesktop.IBus',
      dbus.toObjectPath('/org/freedesktop/IBus'),
      'org.freedesktop.IBus',
      'GlobalEngine'
    ).then(function (result) {
      var imeName = result[3];
      var languageCode = result[5];
      var imeIcon = result[8];
      var imeSymbol = result[12];

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
    command.execute('ibus address').then(function (address) {
      address = address.trim();
      dbus.connect(address).then(function (connectedBus) {
        bus = connectedBus;
        display();
        bus.listen({ member: 'GlobalEngineChanged' }).then(function (evt) {
          evt.add(display);
        });
      });
    });
  });

  return self;
});
