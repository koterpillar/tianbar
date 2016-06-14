/* jshint esversion: 6 */
define(['jquery', './dbus'], function ($, dbus) {
  "use strict";

  const self = {};

  const WIDTH = 25;
  const HEIGHT = 25;

  const INACTIVE_COLOR = '#999';

  self.widget = function () {
    return $('.widget-network');
  };

  self.updated = $.Callbacks();

  const NMActiveConnectionState = {
    Unknown: 0, // the state of the connection is unknown
    Activating: 1, // a network connection is being prepared
    Activated: 2, // there is a connection to the network
    Deactivating: 3, // the network connection is being torn down and cleaned up
    Deactivated: 4 // the network connection is disconnected and will be removed
  };

  const ConnectionType = {
    WiFi: '802-11-wireless',
    // TODO: More types
  };

  function canvas() {
    const canvas = $('<canvas />').attr({
      width: WIDTH,
      height: HEIGHT
    });

    const widget = self.widget();
    widget.empty();
    widget.append(canvas);

    return canvas[0].getContext('2d');
  }

  // Show the wireless signal strength on the canvas
  function wireless(context, strength) {
    const pi = Math.PI;

    const bottomPadding = 6;
    const maxBars = 4;
    const barSlope = pi / 5;

    context.save();

    context.lineWidth = 1.5;

    const barCount = Math.floor(Math.min(strength, 100) * maxBars / 100);
    for (var i = 0; i < barCount; i++) {
      context.beginPath();
      context.arc(
        WIDTH / 2, HEIGHT - bottomPadding,
        4 * i + 2,
        -barSlope, -(pi - barSlope),
        true
      );
      context.stroke();
    }
    context.restore();
  }

  // Show a generic connection on the canvas
  function normal(context, color) {
    const offsetHorizontal = 8;
    const offsetTopBottom = 5;

    const arrowHeadSize = 5;
    const arrowBodyWidth = 3;
    const arrowBodyHeight = 7;

    if (color === undefined) {
      color = '#000000';
    }
    context.save();
    context.fillStyle = color;

    function arrow() {
      context.beginPath();
      context.moveTo(0, 0);
      context.lineTo(arrowHeadSize, arrowHeadSize);
      context.lineTo(arrowBodyWidth / 2, arrowHeadSize);
      context.lineTo(arrowBodyWidth / 2, arrowHeadSize + arrowBodyHeight);
      context.lineTo(-arrowBodyWidth / 2, arrowHeadSize + arrowBodyHeight);
      context.lineTo(-arrowBodyWidth / 2, arrowHeadSize);
      context.lineTo(-arrowHeadSize, arrowHeadSize);
      context.lineTo(0, 0);
      context.fill();
    }

    context.save();
    context.translate((WIDTH - offsetHorizontal) / 2, offsetTopBottom);
    arrow();
    context.restore();

    context.save();
    context.translate((WIDTH + offsetHorizontal) / 2, HEIGHT - offsetTopBottom);
    context.rotate(Math.PI);
    arrow();
    context.restore();
    context.restore();
  }

  // Overlay a progress indication on the canvas
  function overlayProgress(context) {
    context.save();
    context.lineWidth = 1.5;

    for (var i = 0; i < 3; i++) {
      context.save();
      context.globalCompositeOperation = 'destination-out';
      context.fillStyle = 'rgba(255, 255, 255, 0.9)';
      context.beginPath();
      context.arc(WIDTH - 1 - 4 * i, HEIGHT - 9, 3, 0, 2 * Math.PI, true);
      context.fill();
      context.restore();

      context.beginPath();
      context.arc(WIDTH - 1 - 4 * i, HEIGHT - 9, 1, 0, 2 * Math.PI, true);
      context.fill();
    }

    context.restore();
  }

  // Overlay a cross indication on the canvas
  function overlayCross(context) {
    const crossSize = 5;

    const x1 = WIDTH - 1;
    const x2 = x1 - crossSize;
    const y1 = HEIGHT - 5;
    const y2 = y1 - 5;

    context.save();
    context.lineWidth = 1.5;

    context.beginPath();
    context.moveTo(x1, y1);
    context.lineTo(x2, y2);
    context.stroke();

    context.beginPath();
    context.moveTo(x1, y2);
    context.lineTo(x2, y1);
    context.stroke();

    context.restore();
  }

  // Overlay a question indication on the canvas
  function overlayQuestion(context) {
    context.save();

    context.font = '10px sans';

    context.fillText('?', WIDTH - 4, HEIGHT - 5);

    context.restore();
  }

  // Display the network status
  self.display = function () {
    var context = canvas();

    const isConnected = self.connection.type !== null;
    // TODO: Devices have their own types, but there might be more than one of
    // them. Either leave the devices out, or use their types.
    const isWireless = self.connection.type == ConnectionType.WiFi;

    if (!isConnected) {
      normal(context, INACTIVE_COLOR);
    } else if (isWireless) {
      // TODO: Show the wireless signal strength
      wireless(context, 100);
    } else {
      normal(context);
    }

    switch (self.connection.state) {
      case NMActiveConnectionState.Unknown:
        overlayQuestion(context);
        break;
      case NMActiveConnectionState.Activating:
        overlayProgress(context);
        break;
      case NMActiveConnectionState.Activated:
        // Normal state - nothing to do
        break;
      case NMActiveConnectionState.Deactivating:
        overlayCross(context);
        break;
      case NMActiveConnectionState.Deactivated:
        overlayCross(context);
        break;
      default:
        overlayQuestion(context);
    }

    self.updated.fire();
  };

  self.connection = {
    name: null,
    type: null,
    state: NMActiveConnectionState.Unknown,
    devices: {},
  };

  function watch_properties(obj) {
    return dbus.system.listen({
      path: obj,
      iface: 'org.freedesktop.DBus.Properties',
      member: 'PropertiesChanged'
    });
  }

  function refresh_device(conn, device) {
    console.log('refresh_device conn=' + conn + ' dev=' + device);
    if (conn != self.connection.name) {
      // Stale signal from an old connection which isn't the one we want
      // anymore
      return;
    }

    var get_dev_property = (prop) => dbus.system.getProperty(
      'org.freedesktop.NetworkManager',
      device,
      'org.freedesktop.NetworkManager.Device',
      prop
    );

    $.when(
      get_dev_property('DeviceType')
    ).done(function (dev_type) {
      self.connection.devices[device] = {
        type: dev_type
      };
      self.display();
    });
  }

  function refresh_connection(conn) {
    console.log('refresh_connection conn=' + conn);
    if (conn != self.connection.name) {
      // Stale signal from an old connection which isn't the one we want
      // anymore
      return;
    }

    var get_conn_property = (prop) => dbus.system.getProperty(
      'org.freedesktop.NetworkManager',
      conn,
      'org.freedesktop.NetworkManager.Connection.Active',
      prop
    );

    $.when(
      get_conn_property('Type'),
      get_conn_property('State'),
      get_conn_property('Devices')
    ).done(function (conn_type, conn_state, devices) {
      console.log('refresh_connection type=' + conn_type + ' state=' + conn_state + ' devices=' + JSON.stringify(devices));
      self.connection.type = conn_type;
      self.connection.state = conn_state;

      devices.forEach(function (device) {
        watch_properties(device).then(function (evt) {
          evt.add(function () { refresh_device(conn, device); });
        });
        refresh_device(conn, device);
      });

      self.display();
    });
  }

  function subscribe_to_connection(conn) {
    if (conn == '/') {
      // No connection
      self.connection.type = null;
      self.connection.state = NMActiveConnectionState.Deactivated;
      self.display();
    } else {
      watch_properties(conn).then(function (evt) {
        evt.add(function () { refresh_connection(conn); });
      });
      refresh_connection(conn);
    }
  }

  self.refresh = function () {
    console.log('refresh');
    dbus.system.getProperty(
      'org.freedesktop.NetworkManager',
      '/org/freedesktop/NetworkManager',
      'org.freedesktop.NetworkManager',
      'PrimaryConnection'
    ).done(function (conn) {
      if (self.connection.name != conn) {
        console.log('refresh: new conn=' + conn);
        self.connection.name = conn;
        subscribe_to_connection(conn);
      }
    });
  };

  $(document).ready(function () {
    watch_properties('/org/freedesktop/NetworkManager').then(function (evt) {
      evt.add(self.refresh);
    });
    self.refresh();
  });

  return self;
});
