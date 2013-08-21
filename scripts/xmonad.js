/*
 * XMonad status plugin, receiving workspace status over DBus.
 * To send status, use dbusLog or dbusLogWithMarkup from
 * System.Tianbar.XMonadLog as your XMonad logHook.
 *
 * The status appears in elements matching class 'widget-xmonad'.
 *
 * The plugin requires 'jquery' to be available through RequireJS.
 */
define(['jquery'], function ($) {
  function setStatus(st) {
    $('.widget-xmonad').html(st);
  }

  // Currently, a check for this function (and if it doesn't exist,
  // window.XMonadStatus) is hardcoded in Tianbar. A better solution
  // would be to expose an interface for subscribing to DBus connections
  // from JavaScript.
  window.setXMonadStatus = function (st) {
    window.XMonadStatus = undefined;
    setStatus(st);
  };

  $(document).ready(function () {
    window.setTimeout(function () {
      if (window.XMonadStatus) {
        window.setXMonadStatus(window.XMonadStatus);
      }
    }, 1000);
  });
});
