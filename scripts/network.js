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
    for (var i = 0; i < maxBars; i++) {
      context.beginPath();
      if (i >= barCount) {
        context.strokeStyle = INACTIVE_COLOR;
      }
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
    const context = canvas();

    const isConnected = self.connection.type !== null;
    const isWireless = self.connection.type == ConnectionType.WiFi;

    if (!isConnected) {
      normal(context, INACTIVE_COLOR);
    } else if (isWireless) {
      wireless(context, self.connection.strength || 0);
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
  };

  // Remove stale information about the connection
  function reset_connection() {
    self.connection.type = null;
    self.connection.state = NMActiveConnectionState.Unknown;
    self.connection.strength = null;
  }
  reset_connection();

  const get_nm_property = (path, iface, prop) => dbus.system.getProperty(
    'org.freedesktop.NetworkManager', path, iface, prop);

  function watch_properties(path) {
    // NetworkManager doesn't emit PropertiesChanged signal on the
    // obj.freedesktop.DBus.Properties interface, but on its own.
    return dbus.system.listen({
      path: path,
      member: 'PropertiesChanged'
    });
  }

  function refresh_connection_object(conn, conn_object) {
    if (conn != self.connection.name ||
      conn_object != self.connection.specific_object) {
      // Stale signal from an old connection or device
      return;
    }

    const get_obj_property = (iface, prop) => get_nm_property(conn_object, iface, prop);

    if (/AccessPoint/.test(conn_object)) {
      $.when(
        get_obj_property('org.freedesktop.NetworkManager.AccessPoint', 'Strength')
      ).done(function (strength) {
        self.connection.strength = strength;
        self.display();
      });
    }
  }

  function refresh_connection(conn) {
    if (conn != self.connection.name) {
      // Stale signal from an old connection which isn't the one we want
      // anymore
      return;
    }

    const get_conn_property = (prop) => get_nm_property(
      conn,
      'org.freedesktop.NetworkManager.Connection.Active',
      prop
    );

    $.when(
      get_conn_property('Type'),
      get_conn_property('State'),
      get_conn_property('SpecificObject')
    ).done(function (conn_type, conn_state, conn_object) {
      self.connection.type = conn_type;
      self.connection.state = conn_state;
      self.connection.specific_object = conn_object;

      if (conn_object != '/') {
        watch_properties(conn_object).then(function (evt) {
          evt.add(function () {
            refresh_connection_object(conn, conn_object); });
        });
        refresh_connection_object(conn, conn_object);
      }

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
    get_nm_property(
      '/org/freedesktop/NetworkManager',
      'org.freedesktop.NetworkManager',
      'PrimaryConnection'
    ).done(function (conn) {
      if (self.connection.name != conn) {
        self.connection.name = conn;
        reset_connection();
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
