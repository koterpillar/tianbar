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
  }

  $(document).ready(function () {
    var lang = navigator.language;
    if (moment.langData(lang)) {
      moment.lang(lang);
    } else {
      moment.lang(navigator.language.replace(/-.+/, ''));
    }
    updateClock();
    setInterval(updateClock, 1000);
  });

  return config;
});
