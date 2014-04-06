Tianbar
=======

Tianbar is a status bar for XMonad and possibly similar window managers. It is
using WebKit as its rendering engine, meaning that the entire look and feel is
customizable using HTML, CSS and JavaScript.

![Screenshot - left part](tianbar.png) ![Screenshot - right part](tianbar2.png)

Usage
-----

Tianbar will show `index.html` from its [XDG configuration directory][xdg]
(usually `$HOME/.config/taffybar`). It it up to you to create that, add styles,
widgets and other behavior.

Widgets
-------

A small collection of widgets written in JavaScript is bundled with Tianbar,
see `scripts` directory. To include them on your page, use `tianbar:` URL
scheme, e.g. `tianbar:scripts/time.js`.

Currently widgets require [jQuery][jquery] and [RequireJS][requirejs], and
neither of those is distributed with Tianbar -- you will have to obtain them
manually.

XMonad integration
------------------

XMonad status widget works by listening on DBus for messages sent by the
provided `logHook`, see `System.Tianbar.XMonadLog` documentation for details.

Quirks
------

* AJAX requests from Tianbar are not subject to the same origin policy. This
  makes it easier to interact with various Web services. You can also make
  requests to the local files, for example, to extract CPU activity statistics
  from `/proc/stat`.
* HTML5 geolocation API does not work. A shim, which uses
  [freegeoip.net][freegeoip], is provided instead.
* Interaction with the displayed Web page is limited. For example, text fields
  are not active if there are other windows on the screen.

Acknowledgements
----------------

The project is essentially a fork of [Taffybar][taffybar], stripped down of
Haskell configuration and widgets and not yet having achieved functional parity
with it.

[freegeoip]: http://freegeoip.net/
[jquery]: http://jquery.com/
[requirejs]: http://requirejs.org/
[taffybar]: https://github.com/travitch/taffybar
[xdg]: http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
