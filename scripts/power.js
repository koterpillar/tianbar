/*
 * A plugin to show the power (e.g. battery) state through UPower.
 *
 * Requires 'jquery' and 'moment' to be available through RequireJS.
 */
define(['jquery', 'moment', './dbus'], function ($, moment, dbus) {
  var self = {};

  self.HOUR = 3600;

  self.TERMINAL_HEIGHT = 6;
  self.HEIGHT = 8;
  self.WIDTH = 25;

  self.FILL_COLOR = 'black';
  self.FILL_LOW_COLOR = 'red';
  self.EMPTY_COLOR = 'transparent';
  self.SEGMENT_COLOR = '#888';
  self.OUTLINE_COLOR = 'black';

  self.DEFAULT_TIME = 4 * self.HOUR;
  self.LOW_TIME = self.HOUR / 4;

  self.DEVICE_STATE = {
    Unknown: 0,
    Charging: 1,
    Discharging: 2,
    Empty: 3,
    FullyCharged: 4,
    PendingCharge: 5,
    PendingDischarge: 6
  };

  self.DEVICE_TYPE = {
    Unknown: 0,
    LinePower: 1,
    Battery: 2,
    UPS: 3,
    Monitor: 4,
    Mouse: 5,
    Keyboard: 6,
    PDA: 7,
    Phone: 8
  };

  self.block = function (width, color, border, leftBorder) {
    var result = $('<div />');
    result.css({
      'display': 'inline-block',
      'background-color': color,
      'width': width - (border ? 1 : 0),
      'height': self.HEIGHT,
      'border-top': '1px solid black',
      'border-bottom': '1px solid black',
      'margin-top': -1,
      'margin-bottom': -1
    });
    if (border) {
      result.css({
        'padding-right': -1,
        'border-right-width': '1px',
        'border-right-style': 'solid',
        'border-right-color': border,
      });
    }
    if (leftBorder) {
      result.css({
        'border-left-width': '1px',
        'border-left-style': 'solid',
        'border-left-color': leftBorder,
      });
    }
    return result;
  };

  /**
   * Format an HTML element displaying the device status.
   * @param st {Object} The device status, as returned by UPower
   */
  self.formatDevice = function (st) {
    if (st.Type == self.DEVICE_TYPE.LinePower) {
      return '';
    }

    var percentage = st.Percentage;

    var timeToEmpty = st.TimeToEmpty;
    var timeToFull = st.TimeToFull;

    var haveTTE = timeToEmpty !== 0;
    var haveTTF = timeToFull !== 0;

    // Sanity checks if one or the other time is missing
    if (haveTTE && !haveTTF) {
      if (percentage > 0) {
        timeToFull = timeToEmpty * (100 - percentage) / percentage;
      } else {
        timeToFull = self.DEFAULT_TIME;
      }
    } else if (haveTTF && !haveTTE) {
      if (percentage < 100) {
        timeToEmpty = timeToFull * percentage / (100 - percentage);
      } else {
        timeToFull = self.DEFAULT_TIME;
      }
    } else if (!haveTTF && !haveTTE) {
      timeToEmpty = self.DEFAULT_TIME * percentage;
      timeToFull = self.DEFAULT_TIME * (100 - percentage);
    }

    var hour_width = self.WIDTH * self.HOUR / (timeToEmpty + timeToFull);

    var result = $('<div />').css({
      'display': 'inline-block'
    });

    var fillColor = timeToEmpty > self.LOW_TIME ?
      self.FILL_COLOR : self.FILL_LOW_COLOR;
    var firstSegment = true;
    if (haveTTE) {
      for (; timeToEmpty > self.HOUR; timeToEmpty -= self.HOUR) {
        result.append(self.block(
          hour_width,
          fillColor,
          self.SEGMENT_COLOR,
          firstSegment ? self.OUTLINE_COLOR : null
        ));
        firstSegment = false;
      }
    }
    result.append(self.block(
      timeToEmpty / self.HOUR * hour_width,
      fillColor,
      null,
      firstSegment ? self.OUTLINE_COLOR : null
    ));

    if (haveTTF) {
      for (; timeToFull > self.HOUR; timeToFull -= self.HOUR) {
        result.append(self.block(
          hour_width,
          self.EMPTY_COLOR,
          self.SEGMENT_COLOR
        ));
      }
    }
    result.append(self.block(
      timeToFull / self.HOUR * hour_width,
      self.EMPTY_COLOR,
      self.OUTLINE_COLOR
    ));

    result.append($('<span>').css({
      'display': 'inline-block',
      'background-color': self.OUTLINE_COLOR,
      'width': 1,
      'height': self.TERMINAL_HEIGHT,
      'margin-top': (self.HEIGHT - self.TERMINAL_HEIGHT) / 2,
      'margin-bottom': (self.HEIGHT - self.TERMINAL_HEIGHT) / 2
    }));

    var title = percentage + '%';
    if (st.TimeToEmpty !== 0) {
      title += ' (' + moment.duration(st.TimeToEmpty, 's').humanize() + ')';
    }
    if (st.Type != self.DEVICE_TYPE.Battery) {
      title += '\n' + st.Vendor + ' ' + st.Model;
    }

    result.attr('title', title);

    if (st.State === self.DEVICE_STATE.Charging) {
      result.append('⚡');
    }

    return result;
  };

  self.updated = $.Callbacks();

  self.refresh = function () {
    dbus.system.call({
      'destination': 'org.freedesktop.UPower',
      'path': '/org/freedesktop/UPower',
      'iface': 'org.freedesktop.UPower',
      'member': 'EnumerateDevices',
      'body': [
      ]
    }).done(function (devices) {
      devices = devices.body[0];
      var queries = $.map(devices, function(device) {
        return dbus.system.call({
          'destination': 'org.freedesktop.UPower',
          'path': device,
          'iface': 'org.freedesktop.DBus.Properties',
          'member': 'GetAll',
          'body': [
            'string:org.freedesktop.UPower.Device',
          ]
        });
      });
      $.when.apply($, queries).then(function (results) {
        results = Array.prototype.slice.call(arguments, 0);

        var widget = $('.widget-power');
        widget.empty();
        $.each(results, function (_, st) {
          st = st.body[0];
          widget.append(self.formatDevice(st));
        });

        self.updated.fire();
      });
    });
  };

  $(document).ready(function () {
    dbus.system.listen(
      {
        iface: 'org.freedesktop.UPower'
      },
      self.refresh
    );
    self.refresh();
  });

  return self;
});
