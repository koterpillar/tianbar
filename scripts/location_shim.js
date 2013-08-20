/*
 * Basic HTML5 geolocation shim.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery'], function ($) {
  var my_geolocation;

  navigator.geolocation = {};
  navigator.geolocation.getCurrentPosition = function (cb) {
    if (my_geolocation) {
      cb(my_geolocation);
    } else {
      $.getJSON('http://freegeoip.net/json/').success(function (loc) {
        my_geolocation = { coords: loc };
        cb(my_geolocation);
      });
    }
  };
});
