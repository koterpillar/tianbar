/*
 * A plugin to receive DBus events.
 */
define(['jquery'], function ($) {
  return {
    /**
     * Listen for a particular DBus event.
     * @param match {Object} DBus match conditions ('path', 'iface', 'member')
     * @param handler {Function} The function to call upon receiving the event
     */
    listen: function (match, handler) {
      window.dbusCallbacks = window.dbusCallbacks || [];
      var index = window.dbusCallbacks.push(handler) - 1;
      var params = {
        index: index
      };
      for (var x in {'path': undefined,
                     'iface': undefined,
                     'member': undefined}) {
        if (x in match) {
          params[x] = match[x];
        }
      }
      $.ajax('dbus:subscribe', {
        data: params
      });
    }
  };
});
