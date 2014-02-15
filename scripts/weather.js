/*
 * Weather plugin using Open Weather Map.
 *
 * The plugin requires 'jquery' to be available through RequireJS.
 *
 * At least on webkit-0.12.5, geolocation needed by this plugin is not
 * functional, so location_shim.js is required to be loaded before.
 */
define(['jquery'], function ($) {
  var APPID = '41fff6331bf67509b73740427d14c562';

  var SECOND = 1000; // ms
  var MINUTE = 60 * SECOND;
  var HOUR = 60 * MINUTE;

  var coords;
  var weather;
  var forecast;

  function unescapeHTML(html) {
    return $('<span/>').html(html).text();
  }

  function weatherIcon(icon) {
    // TODO: Styles should not be explicitly specified here
    // but are needed to make the image inline
    return $('<img/>')
      .attr('src', 'http://openweathermap.org/img/w/' + icon + '.png')
      .addClass('weather-icon')
      .css({
        'height': '100%',
        'display': 'inline',
        'vertical-align': 'middle',
      })
      [0].outerHTML;
  }

  function weatherUrl(method) {
    return [
      'http://api.openweathermap.org/data/2.5/', method,
      '?APPID=', APPID,
      '&lat=', coords.latitude,
      '&lon=', coords.longitude,
      '&units=', 'metric',
    ].join('');
  }

  function updateWeather() {
    $.ajax(weatherUrl('weather')).success(function (result) {
      weather = result;
      render();
    });
  }

  function updateForecast() {
    $.ajax(weatherUrl('forecast')).success(function (result) {
      forecast = result;
      render();
    });
  }

  function render() {
    var text = [];
    var tooltip = [];
    if (weather) {
      text.push(weatherIcon(weather.weather[0].icon));
      text.push(Math.round(weather.main.temp));
      text.push(" &deg;C");
    }
    if (forecast) {
      // TODO: parse and format per-day (?) forecast
    }
    $('.widget-weather').html(text.join(''));
    $('.widget-weather').attr('title', unescapeHTML(tooltip.join('')));
  }

  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(function (pos) {
      coords = pos.coords;
      updateWeather();
      updateForecast();
      setInterval(updateWeather, 10 * MINUTE);
      setInterval(updateForecast, 3 * HOUR);
    });
  });
});
