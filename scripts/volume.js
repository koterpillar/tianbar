/*
 * A widget to show and control the system volume.
 */
define(['jquery', './socket'], function ($, socket) {
  "use strict";

  var UID_RE = /Uid:\t(\d+)/;

  var VOLUME_RE = /set-sink-volume.+ ([^ \n]+)/;
  var MUTE_RE = /set-sink-mute.+ ([^ \n]+)/;

  var MAX_VOLUME = 0x10000;

  function uid() {
    return $.ajax('/proc/self/status')
    .then(function (result) {
      return +UID_RE.exec(result)[1];
    });
  }

  uid().done(function (uid) {
    var pulseSocket = socket('/var/run/user/' + uid + '/pulse/cli');
    pulseSocket.recv.add(function (dump) {
      var volume = parseInt(VOLUME_RE.exec(dump)[1], 16) / MAX_VOLUME;
      var mute = MUTE_RE.exec(dump)[1] === "on";

      console.log(volume);
      console.log(mute);
    });
    window.setInterval(function () {
      pulseSocket.send('dump\n');
    }, 1000);
  });
});
