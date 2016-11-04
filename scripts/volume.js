/*
 * A widget to show and control the system volume.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  "use strict";

  const MAX_VOLUME = 0x10000;

  const WIDTH = 25;
  const HEIGHT = 25;

  const WAVES = 5;

  const self = {};

  self.settings_command = 'gnome-control-center sound';

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

  $.ajax('tianbar:///execute', {
    data: {
      command: 'pacmd load-module module-dbus-protocol'
    }
  }).then(function () {
    return dbus.session.getProperty(
      'org.PulseAudio1',
      '/org/pulseaudio/server_lookup1',
      'org.PulseAudio.ServerLookup1',
      'Address'
    );
  }).then(function (bus_address) {
    return dbus.connect(bus_address);
  }).then(function (bus) {
    function refresh() {
      function get_device_property(sink, name) {
        return bus.getProperty(
          'org.PulseAudio.Core1',
          sink,
          'org.PulseAudio.Core1.Device',
          name
        );
      }

      bus.getProperty(
        'org.PulseAudio.Core1',
        '/org/pulseaudio/core1',
        'org.PulseAudio.Core1',
        'Sinks'
      ).then(function (sinks) {
        if (sinks.length === 0) {
          // No sound
          return [[0], true];
        } else {
          return $.when(
            get_device_property(sinks[0], 'Volume'),
            get_device_property(sinks[0], 'Mute')
          );
        }
      }).then(function (volumes, mute) {
        // volume is an array of channels' volumes, average it
        const volume = volumes.reduce((a, b) => a + b, 0) / volumes.length;
        self.volume = volume / MAX_VOLUME;
        self.mute = mute;
        self.display();

        window.setTimeout(refresh, 1000);
      });
    }

    refresh();
  });

  $(document).ready(function () {
    self.widget().click(function () {
      $.ajax('tianbar:///spawn', {
        data: {
          command: self.settings_command
        }
      });
    });
  });

  return self;
});
