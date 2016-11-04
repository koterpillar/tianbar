/*
 * A plugin to show the power (e.g. battery) state through UPower.
 *
 * Requires 'jquery' and 'moment' to be available through RequireJS.
 */
define(['jquery', 'moment', './dbus'], function ($, moment, dbus) {
  "use strict";

  const self = {};

  self.HOUR = 3600;

  self.TERMINAL_HEIGHT = 6;
  self.HEIGHT = 8;
  self.WIDTH = 25;

  self.FILL_COLOR = null;
  self.FILL_LOW_COLOR = 'red';
  self.EMPTY_COLOR = 'transparent';
  self.SEGMENT_COLOR = '#888';
  self.OUTLINE_COLOR = null;

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

  self.DISPLAY_DEVICE = dbus.toObjectPath('/org/freedesktop/UPower/devices/DisplayDevice');

  self.widget = () => $('.widget-power');

  self.fill_color = () => self.FILL_COLOR || self.widget().css('color');

  self.outline_color = () => self.OUTLINE_COLOR || self.widget().css('color');

  self.block = function (width, color, border, leftBorder) {
    const result = $('<div />');
    result.css({
      'display': 'inline-block',
      'background-color': color,
      'width': width - (border ? 1 : 0),
      'height': self.HEIGHT,
      'border-top': '1px solid',
      'border-bottom': '1px solid',
      'border-color': self.outline_color(),
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
  self.formatDevice = function (path, st) {
    // Ignore line power
    if (st.Type == self.DEVICE_TYPE.LinePower) {
      return '';
    }

    // Ignore the batteries which are not the (combined) display device
    if (st.Type == self.DEVICE_TYPE.Battery &&
        dbus.fromObjectPath(path) !== dbus.fromObjectPath(self.DISPLAY_DEVICE)) {
      return '';
    }

    var percentage = st.Percentage;

    var displayPercentage = percentage;
    if (st.State == self.DEVICE_STATE.FullyCharged) {
      displayPercentage = 100;
    }

    var timeToEmpty = st.TimeToEmpty;
    var timeToFull = st.TimeToFull;

    var haveTTE = timeToEmpty !== 0;
    var haveTTF = timeToFull !== 0;

    // Sanity checks if one or the other time is missing
    if (haveTTE && !haveTTF) {
      if (displayPercentage > 0) {
        timeToFull = timeToEmpty *
          (100 - displayPercentage) / displayPercentage;
      } else {
        timeToFull = self.DEFAULT_TIME;
      }
    } else if (haveTTF && !haveTTE) {
      if (displayPercentage < 100) {
        timeToEmpty = timeToFull *
          displayPercentage / (100 - displayPercentage);
      } else {
        timeToFull = self.DEFAULT_TIME;
      }
    } else if (!haveTTF && !haveTTE) {
      timeToEmpty = self.DEFAULT_TIME * displayPercentage;
      timeToFull = self.DEFAULT_TIME * (100 - displayPercentage);
    }

    const hour_width = self.WIDTH * self.HOUR / (timeToEmpty + timeToFull);

    const result = $('<div />').css({
      'display': 'inline-block'
    });

    const fillColor = timeToEmpty > self.LOW_TIME ?
      self.fill_color() : self.FILL_LOW_COLOR;
    var firstSegment = true;
    if (haveTTE) {
      for (; timeToEmpty > self.HOUR; timeToEmpty -= self.HOUR) {
        result.append(self.block(
          hour_width,
          fillColor,
          self.SEGMENT_COLOR,
          firstSegment ? self.outline_color() : null
        ));
        firstSegment = false;
      }
    }
    result.append(self.block(
      timeToEmpty / self.HOUR * hour_width,
      fillColor,
      null,
      firstSegment ? self.outline_color() : null
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
      'background-color': self.outline_color(),
      'width': 1,
      'height': self.TERMINAL_HEIGHT,
      'margin-top': (self.HEIGHT - self.TERMINAL_HEIGHT) / 2,
      'margin-bottom': (self.HEIGHT - self.TERMINAL_HEIGHT) / 2,
      'margin-right': 4,
    }));

    var title;
    if (st.State === self.DEVICE_STATE.FullyCharged) {
      title = '';
    } else {
      title = Math.round(percentage) + '%';
      if (st.TimeToEmpty !== 0) {
        title += ' (' + moment.duration(st.TimeToEmpty, 's').humanize() + ')';
      }
    }

    if (st.Type != self.DEVICE_TYPE.Battery) {
      title += '\n' + st.Vendor + ' ' + st.Model;
    }

    title = title.trim();
    result.attr('title', title);

    if (st.State === self.DEVICE_STATE.Charging) {
      result.append('⚡');
    }

    return result;
  };

  self.updated = $.Callbacks();

  // A list of devices currently active
  self.devices = [];

  // A map of device paths to device states
  self.deviceProperties = {};

  // Display the status of all the devices
  self.display = function () {
    const widget = self.widget();

    widget.empty();
    self.devices.forEach(function (path) {
      if (!self.deviceProperties[dbus.fromObjectPath(path)]) {
        // Device is active but no data yet
        return;
      }
      widget.append(self.formatDevice(path, self.deviceProperties[dbus.fromObjectPath(path)]));
    });

    self.updated.fire();
  };

  // Refresh the device list and each device status
  self.refresh = function () {
    dbus.system.call({
      'destination': 'org.freedesktop.UPower',
      'path': dbus.toObjectPath('/org/freedesktop/UPower'),
      'iface': 'org.freedesktop.UPower',
      'member': 'EnumerateDevices',
      'body': [
      ]
    }).done(function (devices) {
      // Add the display device
      devices.push(self.DISPLAY_DEVICE);

      self.devices = devices;

      self.devices.forEach(function (path) {
        self.refreshDevice(path);
      });
    });
  };

  // Refresh an individual device
  self.refreshDevice = function (path) {
    dbus.system.getAllProperties(
      'org.freedesktop.UPower',
      path,
      'org.freedesktop.UPower.Device'
    ).done(function (properties) {
      if (!self.deviceProperties[dbus.fromObjectPath(path)]) {
        // New device never seen before, listen for changes
        dbus.system.listen(
          { path: path }
        ).then(function (evt) {
          evt.add(function () {
            self.refreshDevice(path);
          });
        });
      }

      self.deviceProperties[dbus.fromObjectPath(path)] = properties;
      self.display();
    });
  };

  $(document).ready(function () {
    dbus.system.listen(
      { path: '/org/freedesktop/UPower' }
    ).then(function (evt) {
      evt.add(self.refresh);
    });
    self.refresh();
  });

  return self;
});
