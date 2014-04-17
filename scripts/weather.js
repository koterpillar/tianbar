/*
 * Weather plugin using Open Weather Map.
 *
 * Requires 'jquery' and 'moment' to be available through RequireJS.
 */
define(['jquery', 'moment'], function ($, moment) {
  "use strict";
  var APPID = '41fff6331bf67509b73740427d14c562';

  var SECOND = 1000; // ms
  var MINUTE = 60 * SECOND;
  var HOUR = 60 * MINUTE;

  var coords;
  var weather;
  var forecast;
  var forecastWindow = null;

  function unescapeHTML(html) {
    return $('<span/>').html(html).text();
  }

  function weatherIcon(data, doc) {
    // TODO: Why would there be multiple weather?
    var weather = data.weather[0];
    // TODO: Styles should not be explicitly specified here
    // but are needed to make the image inline
    return $('<img/>', doc || document)
      .attr('src', 'http://openweathermap.org/img/w/' + weather.icon + '.png')
      .attr('title', weather.description)
      .addClass('weather-icon')
      .css({
        'height': '1.5em',
        'display': 'inline',
        'vertical-align': 'middle'
      });
  }

  function weatherUrl(method) {
    return [
      'http://api.openweathermap.org/data/2.5/', method,
      '?APPID=', APPID,
      '&lat=', coords.latitude,
      '&lon=', coords.longitude,
      '&units=', 'metric',
      '&lang=', navigator.language,
      ''
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

  function formatTemperature(temp, temp2) {
    var result = Math.round(temp);
    if (temp2 !== undefined) {
      result += '..' + Math.round(temp2);
    }
    result += " &deg;C";
    return result;
  }

  function getWidget() {
    return $('.widget-weather');
  }

  function showForecast() {
    if (forecastWindow === null || forecastWindow.closed) {
      var widget = getWidget();
      var offset = widget.offset();
      forecastWindow = window.open('about:blank', 'forecast',
        [
          'left=' + offset.left,
          'top=' + offset.top,
          ''
        ].join(';'));
    } else {
      forecastWindow.close();
    }

    render();
  }

  function render() {
    var text = [];
    var tooltip = [];
    if (weather) {
      text.push(weatherIcon(weather));
      text.push(formatTemperature(weather.main.temp));

      tooltip.push(weather.name);
    }
    if (forecastWindow !== null && !forecastWindow.closed) {
      var forecastBody = $(forecastWindow.document.body);
      forecastBody.empty();
      if (forecast) {
        // Split forecast by days
        var days = [];
        var daily = [];
        var prevDate = null;
        for (var i in forecast.list) {
          var fc = forecast.list[i];
          var fcTime = new Date(fc.dt * SECOND);
          var fcDate = new Date(fcTime);
          fcDate.setHours(0, 0, 0, 0);
          if (prevDate && prevDate.getTime() != fcDate.getTime()) {
            // A new day has started, dump all daily forecast
            days.push(daily);
            daily = [];
          } else {
            daily.push(fc);
          }
          prevDate = fcDate;
        }
        if (daily.length > 0) {
          days.push(daily);
        }

        for (var d in days) {
          // Parse forecast: minimum and maximum for each day
          var day = days[d];
          var dailyEl = $('<p/>', forecastBody)
            .addClass('forecast-day');
          dailyEl.append(
            moment(new Date(day[0].dt * SECOND)).format('DD MMM'));
          var minTemp = null, maxTemp = null;
          var dfc;
          for (var j in day) {
            dfc = day[j];
            if (minTemp === null || minTemp > dfc.main.temp) {
              minTemp = dfc.main.temp;
            }
            if (maxTemp === null || maxTemp < dfc.main.temp) {
              maxTemp = dfc.main.temp;
            }
          }
          dailyEl.append(' ' + formatTemperature(minTemp, maxTemp));
          dailyEl.append($('<br/>', forecastBody));
          for (j in day) {
            dfc = day[j];
            dailyEl.append(weatherIcon(dfc));
          }
          forecastBody.append(dailyEl);
        }
      }
    }

    var widget = getWidget();
    widget.empty();
    text.forEach(function (textEl) {
      widget.append(textEl);
    });
    widget.attr('title', unescapeHTML(tooltip.join('')));
  }

  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(function (pos) {
      coords = pos.coords;
      updateWeather();
      updateForecast();
      setInterval(updateWeather, 10 * MINUTE);
      setInterval(updateForecast, 3 * HOUR);
    });
    $('.widget-weather').click(showForecast);
  });
});
