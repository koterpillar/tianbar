/*
 * A plugin to receive DBus events.
 */
define(['jquery'], function ($) {
  function copyProperties(properties, from, to) {
    $.each(properties, function(_, x) {
      if (x in from) {
        to[x] = from[x];
      }
    });
  }

  return {
    /**
     * Listen for a particular DBus event.
     * @param match {Object} DBus match conditions ('path', 'iface', 'member')
     * @param handler {Function} The function to call upon receiving the event
     */
    listen: function (match, handler) {
      window.dbusCallbacks = window.dbusCallbacks || [];
      var index = window.dbusCallbacks.push(handler) - 1;
      var data = {
        index: index
      };
      copyProperties(['path', 'iface', 'member'], match, data);
      $.ajax('dbus:listen', {
        data: data
      });
    },

    /**
     * Call a DBus method.
     * @param params {Object} Method call details ('path', 'iface', 'member',
     * 'destination', 'body')
     * @param handler {Function} The function to call upon receiving the result
     */
    call: function (params, handler) {
      var data = {};
      copyProperties(
          ['path', 'iface', 'member', 'destination', 'body'], params, data);
      $.ajax('dbus:call', {
        data: data
      }).done(function (result) {
        console.log(result);
      });
    }
  };
});
