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

  function bus(busName) {
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
        $.ajax('dbus:' + busName + '/listen', {
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
        $.ajax('dbus:' + busName + '/call', {
          data: data
        }).done(function (result) {
          result = JSON.parse(result);
          handler(result);
        });
      }
    };
  }

  return {
    /**
     * Session bus.
     */
    session: bus('session'),
    /**
     * System bus.
     */
    system: bus('system')
  };
});
