/*
 * A widget to show and control the system volume.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './socket'], function ($, socket) {
  "use strict";

  var UID_RE = /Uid:\t(\d+)/;

  var VOLUME_RE = /set-sink-volume.+ ([^ \n]+)/;
  var MUTE_RE = /set-sink-mute.+ ([^ \n]+)/;

  var MAX_VOLUME = 0x10000;

  var WAVES = 5;

  function uid() {
    return $.ajax('tianbar:///root/proc/self/status')
    .then(function (result) {
      return +UID_RE.exec(result)[1];
    });
  }

  $.when(
    $.ajax('tianbar:///execute', {
      data: {
        command: 'pacmd load-module module-cli-protocol-unix'
      }
    }),
    uid()
  ).then(function (_, uid) {
    return socket('/var/run/user/' + uid + '/pulse/cli');
  }).then(function (pulseSocket) {
    pulseSocket.recv.add(function (dump) {
      var mute = MUTE_RE.exec(dump)[1] === "yes";
      var volume = parseInt(VOLUME_RE.exec(dump)[1], 16) / MAX_VOLUME;

      var widget = $('.widget-volume');
      widget.empty();

      var speaker = $('<div />').css({
        'float': 'left',
        'border-top': '4px solid transparent',
        'border-bottom': '4px solid transparent',
        'border-right': '5px solid black',
        'margin-top': 5,
        'margin-bottom': 5,
        'margin-right': 2
      }).append($('<div />').css({
        'background': 'black',
        'width': 4,
        'height': 5
      }));
      widget.append(speaker);

      var waves = $('<div />').css({
        'float': 'left',
        'height': '100%',
        'margin-top': 5,
        'margin-bottom': 5,
        'width': 12
      });
      if (mute) {
        waves.append('x');
      } else {
        for (var i = 0; i < volume * WAVES; i ++) {
          var waveSize = 6 + i * 2;
          var margin = 10 - waveSize / 2;
          var wave = $('<div />').css({
            'display': 'inline-block',
            'width': 1,
            'height': waveSize,
            'margin': 1,
            'background': 'black',
            'margin-bottom': margin
          });
          waves.append(wave);
        }
      }
      widget.append(waves);

      var percentage = volume.toFixed(2) * 100;
      widget.attr('title', percentage + '%');

      window.setTimeout(requestDump, 1000);
    });

    function requestDump () {
      pulseSocket.send('dump\n');
    }

    requestDump();
  });
});
