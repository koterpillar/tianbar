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
      var html = '';

      var mute = MUTE_RE.exec(dump)[1] === "on";

      // TODO: Volume icons
      html += '<';
      if (mute) {
        html += 'x';
      } else {
        var volume = parseInt(VOLUME_RE.exec(dump)[1], 16) / MAX_VOLUME;
        for (var wave = 0; wave < volume; wave += 1/3) {
          html += ')';
        }
      }

      $('.widget-volume').html(html);
    });

    function requestDump () {
      pulseSocket.send('dump\n');
    }

    requestDump();
    window.setInterval(requestDump, 1000);
  });
});
