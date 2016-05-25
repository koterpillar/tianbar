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

  // Display the status of the input method
  function display(engine) {
    var widget = self.widget();

    widget.text(engine);
    self.updated.fire(engine);
  }

  $(document).ready(function () {
    $.ajax('tianbar:///execute', {
      data: {
        command: 'ibus address'
      }
    }).then(function (address) {
      address = address.trim();
      dbus.connect(address).then(function (bus) {
        bus.listen({ member: 'GlobalEngineChanged' }).add(function (result) {
          var engine = result.body[0];
          display(engine);
        });
      });
    });
  });

  return self;
});
