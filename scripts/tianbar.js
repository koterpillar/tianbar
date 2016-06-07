/*
 * Support infrastructure for Tianbar.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery'], function ($) {
  "use strict";
  var events = window.tianbarEvents = window.tianbarEvents || [];

  /**
   * Create a new server event.
   * @param response {Object} Server response with the callback index
   * @returns {Object} 'callback' for the jQuery callback object; 'callbackIndex' for the event index to supply to Tianbar
   */
  function createEvent(response) {
    var callbackIndex = JSON.parse(response).callbackIndex;

    var evt = events[callbackIndex] = $.Callbacks();

    return {
      callback: evt,
      callbackIndex: callbackIndex
    };
  }

  return {
    createEvent: createEvent
  };
});
