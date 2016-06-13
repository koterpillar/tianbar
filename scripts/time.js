/*
 * Clock plugin using Moment.js (http://momentjs.com/) to
 * format the time and date.
 *
 * The only exposed property, 'format', determines the
 * format (see Moment.js documentation) to display time and date.
 *
 * Requires 'jquery' and 'moment' to be available through RequireJS.
 */
define(['jquery', 'moment'], function ($, moment) {
  "use strict";
  var config = {
    format: 'llll'
  };

  function updateClock() {
    var dt = moment();
    var clockText = dt.format(config.format);
    $('.widget-time').text(clockText);

    var interval = 59 - dt.second();
    if (interval < 1) {
      interval = 1;
    } else if (interval > 5) {
      interval = 5;
    }
    setTimeout(updateClock, interval * 1000);
  }

  $(document).ready(function () {
    var lang = navigator.language;
    if (moment.localeData(lang)) {
      moment.locale(lang);
    } else {
      moment.locale(navigator.language.replace(/-.+/, ''));
    }
    updateClock();
  });

  return config;
});
