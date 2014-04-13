/*
 * A plugin to connect to UNIX domain sockets.
 */
define(['jquery', './tianbar'], function ($, tianbar) {
  "use strict";
  /**
   * Connect to a UNIX domain socket.
   * @param path {String} Socket path
   * @return {Object} A socket object
   */
  return function (path) {
    var evt = tianbar.createEvent();

    return $.ajax('tianbar:///socket/connect', {
      data: {
        callbackIndex: evt.index,
        path: path,
        random: new Date().getTime()
      }
    }).then(function () {
      return {
        /**
         * Send data to a socket.
         * @param data {String} The data to send.
         */
        send: function (data) {
          $.ajax('tianbar:///socket/send', {
            data: {
              callbackIndex: evt.index,
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
              callbackIndex: evt.index,
              random: new Date().getTime()
            }
          });
        }
      };
    });
  };
});
