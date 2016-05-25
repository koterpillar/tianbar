/*
 * A plugin to receive DBus events.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './tianbar'], function ($, tianbar) {
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
       * @returns {Event} A Tianbar event object
       */
      listen: function (match) {
        var evt = tianbar.createEvent();
        var data = {
          index: evt.index
        };
        copyProperties(['path', 'iface', 'member'], match, data);
        $.ajax('tianbar:///dbus/' + busName + '/listen', {
          data: data
        });

        return evt.callback;
      },

      /**
       * Stop listening for a DBus event.
       * @param evt {Event} A Tianbar event object returned by listen().
       */
      removeMatch: function (evt) {
        $.ajax('tianbar:///dbus/' + busName + '/stop', {
          data: {
            index: evt.index
          }
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

        $.ajax('tianbar:///dbus/' + busName + '/call', {
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

  /**
   * Connect to an arbitrary bus.
   * @param address {String} Bus address
   * 'destination', 'body')
   * @return {Deferred} A promise to be fulfilled with the bus object
   */
  function connectBus(address) {
    // random name
    var name = new Date().getTime();

    return $.ajax('tianbar:///dbus/connect', {
      data: {
        name: name,
        address: address
      }
    }).then(function () {
      return bus(name);
    });
  }

  return {
    /**
     * Session bus.
     */
    session: bus('session'),
    /**
     * System bus.
     */
    system: bus('system'),
    /**
     * Connect an arbitrary bus.
     */
    connect: connectBus
  };
});
