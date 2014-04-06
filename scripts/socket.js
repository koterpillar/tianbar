/*
 * A plugin to connect to UNIX domain sockets.
 */
define(['jquery', './tianbar'], function ($, tianbar) {
  "use strict";
  return function (path) {
    var evt = tianbar.createEvent();

    var socketId;

    return {
      send: function (data) {
        // TODO: only sends data once
        var connectCall = $.ajax('socket:connect', {
          data: {
            callbackIndex: evt.index,
            path: path,
            data: data
          }
        });
        connectCall.done(function (result) {
          socketId = result;
        });
      },
      recv: evt.callback
      // TODO: close sockets
    };
  };
});
