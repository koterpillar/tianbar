/*
 * Support infrastructure for Tianbar
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

    var index = events.push(evt) - 1;

    return {
      index: index,
      callback: evt
    };
  }

  return {
    createEvent: createEvent
  };
});
