/*
 * Support infrastructure for Tianbar.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery'], function ($) {
  /**
   * Create a new server event.
   * @return {Object} 'index' for the event index to supply to Tianbar; 'callback' for the jQuery callback object
   */
  "use strict";
  var events = window.tianbarEvents = window.tianbarEvents || [];

  function createEvent() {
    var evt = $.Callbacks();

    var index = Math.random();
    events[index] = evt;

    return {
      index: index,
      callback: evt
    };
  }

  return {
    createEvent: createEvent
  };
});
