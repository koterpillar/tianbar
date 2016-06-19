/* jshint esversion: 6 */
/*
 * A widget to show and control the system volume.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './socket'], function ($, socket) {
  "use strict";

  const UID_RE = /Uid:\t(\d+)/;

  const VOLUME_RE = /set-sink-volume.+ ([^ \n]+)/;
  const MUTE_RE = /set-sink-mute.+ ([^ \n]+)/;

  const MAX_VOLUME = 0x10000;

  const WIDTH = 25;
  const HEIGHT = 25;

  const WAVES = 5;

  const self = {};

  self.settings_command = 'gnome-control-center sound &';

  function uid() {
    return $.ajax('tianbar:///root/proc/self/status')
    .then(function (result) {
      return +UID_RE.exec(result)[1];
    });
  }

  self.widget = () => $('.widget-volume');

  self.display = function () {
    const widget = self.widget();

    const canvas = $('<canvas />').attr({
      width: WIDTH,
      height: HEIGHT
    });

    widget.empty();
    widget.append(canvas);

    const context = canvas[0].getContext('2d');

    const middle = WIDTH / 2 - 2;

    const speakerBaseH = 5;
    const speakerBaseW = 4;
    const speakerW = 5;
    const speakerH = 13;

    const spacing = 2;

    const crossSize = 6;

    // Draw a speaker
    context.beginPath();
    context.moveTo(0, middle - speakerBaseH / 2);
    context.lineTo(0, middle + speakerBaseH / 2);
    context.lineTo(speakerBaseW, middle + speakerBaseH / 2);
    context.lineTo(speakerBaseW + speakerW, middle + speakerH / 2);
    context.lineTo(speakerBaseW + speakerW, middle - speakerH / 2);
    context.lineTo(speakerBaseW, middle - speakerBaseH / 2);
    context.lineTo(0, middle - speakerBaseH / 2);
    context.fill();

    if (self.mute) {
      // Draw a cross
      const crossLeft = speakerBaseW + speakerW + spacing;

      context.save();
      context.lineWidth = 1.5;

      context.beginPath();
      context.moveTo(crossLeft, middle - crossSize / 2);
      context.lineTo(crossLeft + crossSize, middle + crossSize / 2);
      context.stroke();

      context.beginPath();
      context.moveTo(crossLeft + crossSize, middle - crossSize / 2);
      context.lineTo(crossLeft, middle + crossSize / 2);
      context.stroke();

      context.restore();
    } else {
      // Draw waves
      const waveAngle = Math.PI / 6;
      for (var i = 0; i < self.volume * WAVES; i ++) {
        context.beginPath();
        context.arc(
          0, middle,
          speakerBaseW + speakerW + spacing + 3 * i,
          waveAngle / 2, -waveAngle / 2,
          true);
        context.stroke();
      }
    }

    const percentage = self.volume.toFixed(2) * 100;
    widget.attr('title', percentage + '%');
  };

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
      self.mute = MUTE_RE.exec(dump)[1] === "yes";
      self.volume = parseInt(VOLUME_RE.exec(dump)[1], 16) / MAX_VOLUME;

      self.display();

      window.setTimeout(requestDump, 1000);

    });

    function requestDump () {
      pulseSocket.send('dump\n');
    }

    requestDump();
  });

  $(document).ready(function () {
    self.widget().click(function () {
      $.ajax('tianbar:///execute', {
        data: {
          command: self.settings_command
        }
      });
    });
  });

  return self;
});
