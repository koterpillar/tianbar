/*
 * A plugin to receive DBus events.
 */
define(['jquery'], function ($) {
  "use strict";
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
        window.tianbarCallbacks = window.tianbarCallbacks || [];
        var index = window.tianbarCallbacks.push(handler) - 1;
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
       * @return {Deferred} A promise to be fulfilled or rejected with the result
       */
      call: function (params) {
        var data = {};
        copyProperties(
            ['path', 'iface', 'member', 'destination', 'body'], params, data);
        // Prevent caching
        data.random = new Date().getTime();
        var deferred = $.Deferred();

        $.ajax('dbus:' + busName + '/call', {
          data: data
        }).done(function (result) {
          result = JSON.parse(result);
          if (result.Right) {
            deferred.resolve(result.Right);
          } else {
            deferred.reject(result.Left);
          }
        });

        return deferred;
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
