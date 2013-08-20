/*
 * Weather plugin using Yahoo! Weather API.
 *
 * The plugin requires 'jquery' to be available through RequireJS.
 *
 * At least on webkit-0.12.4, geolocation needed by this plugin is not
 * functional, so location_shim.js is required to be loaded before.
 */
define(['jquery'], function ($) {
  var api_key = 'a54ebf597a8e77198d9a47c798d42c51';
  var woeid;

  function unescapeHTML(html) {
    return $('<span/>').html(html).text();
  }

  function updateWeather() {
    $.ajax('http://weather.yahooapis.com/forecastrss?' +
      'w=' + woeid + '&u=c')
    .success(function (weather) {
      var temp = $('condition', weather).attr('temp');
      var units = $('units', weather).attr('temperature');
      units = " &deg;" + units;
      var forecast = $('forecast', weather).map(function (k, el) {
        el = $(el);
        return el.attr('day') + ": " + el.attr('text') + ", " +
          el.attr('low') + "&ndash;" + el.attr('high') +
          units;
      });
      forecast = Array.prototype.join.call(forecast, '&#13;');
      $('.widget-weather').html(temp + units);
      $('.widget-weather').attr('title', unescapeHTML(forecast));
    });
  }

  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(function (pos) {
      $.getJSON('http://query.yahooapis.com/v1/public/yql?q=' +
        'select%20place.woeid%20from%20flickr.places%20where%20' +
        'lat%3D' + pos.coords.latitude + '%20and%20' +
        'lon%3D' + pos.coords.longitude + '%20and%20' +
        'api_key=%22' + api_key + '%22&format=json')
      .success(function (yloc) {
        woeid = yloc.query.results.places.place.woeid;
        updateWeather();
        setInterval(updateWeather, 5 * 60 * 1000);
      });
    });
  });
});
