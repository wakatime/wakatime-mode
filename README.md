# Emacs Minor Mode for WakaTime

`wakatime-mode` is an automatic time tracking extension for Emacs using [WakaTime](https://wakatime.com/).


## Installation

Heads Up! WakaTime depends on [Python](http://www.python.org/getit/) being installed to work correctly.

1. Install wakatime-mode for Emacs using [MELPA](https://melpa.org/#/wakatime-mode) (Doom users see [these instructions][doom install] instead).

2. Install [wakatime-cli](https://pypi.python.org/pypi/wakatime) with `pip install wakatime`.

3. Add `(global-wakatime-mode)` to your `init.el` file, then restart Emacs.

4. You will see a prompt asking for the path to wakatime-cli. Run `which wakatime` and enter that path into the emacs prompt, then press `enter`.

5. Enter your [api key](https://wakatime.com/settings#apikey) in your `init.el` or `~/.wakatime.cfg` file.

6. Use Emacs with wakatime-mode turned on and your time will be tracked for you automatically.

7. Visit http://wakatime.com to see your logged time.


## Screen Shots

![Project Overview](https://wakatime.com/static/img/ScreenShots/ScreenShot-2014-10-29.png)


## Usage

Enable WakaTime for the current buffer by invoking `M-x wakatime-mode`.  If you wish to activate it globally, run `M-x global-wakatime-mode`.


## Configuration

Set variable `wakatime-api-key` to your [API key](https://wakatime.com/#apikey).

Point `wakatime-cli-path` to the absolute path of [wakatime-cli](https://pypi.python.org/pypi/wakatime).

Optionally, point `wakatime-python-bin` to the absolute path of python on your system. Defaults to `python` which only works if python is in your PATH.


## Troubleshooting

To be sure heartbeats are getting sent, turn on debug mode by adding this line to your `~/.wakatime.cfg` file:

    debug = true

Then run `tail -f ~/.wakatime.log` and make sure you see a 201 response code from the [WakaTime API](https://wakatime.com/api).

[doom install]: https://medium.com/@el.gamerph/how-to-install-wakatime-in-doom-emacs-e5c582e15261
