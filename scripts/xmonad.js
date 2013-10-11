/*
 * XMonad status plugin, receiving workspace status over DBus.
 * To send status, use dbusLog or dbusLogWithMarkup from
 * System.Tianbar.XMonadLog as your XMonad logHook.
 *
 * The status appears in elements matching class 'widget-xmonad'.
 *
 * The plugin requires 'jquery' to be available through RequireJS.
 */
define(['jquery', './dbus'], function ($, dbus) {
  dbus.listen(
    {
      path: '/org/xmonad/Log',
      iface: 'org.xmonad.Log',
      member: 'Update'
    },
    function (signal, body) {
      var st = body[0];
      $('.widget-xmonad').html(st);
    }
  );
});
