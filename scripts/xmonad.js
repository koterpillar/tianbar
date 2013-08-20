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
