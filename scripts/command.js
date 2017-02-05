/*
 * A plugin to execute commands.
 *
 * Requires 'jquery' to be available through RequireJS.
 */
define(['jquery'], function ($) {
  "use strict";
  return {
    /**
     * Spawn a command, do not wait for completion.
     * @param {String} command Command to launch
     */
    spawn: function (command) {
      $.ajax('tianbar:///spawn', {
        data: {
          command: command,
          random: new Date().getTime()
        }
      });
    },

    /**
     * Execute a command.
     * @param {String} command Command to execute
     * @return {String} Command output
     */
    execute: function (command) {
      return $.ajax('tianbar:///execute', {
        data: {
          command: command,
          random: new Date().getTime()
        }
      });
    }
  };
});
