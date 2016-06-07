/*
 * A plugin to connect to UNIX domain sockets.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './tianbar'], function ($, tianbar) {
  "use strict";
  /**
   * Connect to a UNIX domain socket.
   * @param path {String} Socket path
   * @return {Object} A socket object
   */
  return function (path) {
    return $.ajax('tianbar:///socket/connect', {
      data: {
        path: path,
        random: new Date().getTime()
      }
    }).then(tianbar.createEvent).then(function (evt) {
      return {
        /**
         * Send data to a socket.
         * @param data {String} The data to send.
         */
        send: function (data) {
          $.ajax('tianbar:///socket/send', {
            data: {
              callbackIndex: evt.callbackIndex,
              data: data,
              random: new Date().getTime()
            }
          });
        },
        /**
         * Event (jQuery.Callbacks) firing on received data.
         */
        recv: evt.callback,
        /**
         * Close a socket.
         */
        close: function () {
          $.ajax('tianbar:///socket/close', {
            data: {
              callbackIndex: evt.callbackIndex,
              random: new Date().getTime()
            }
          });
        }
      };
    });
  };
});
