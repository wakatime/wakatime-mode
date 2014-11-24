# Emacs Minor Mode for WakaTime

`wakatime-mode` is an automatic time tracking extension for Emacs using [WakaTime](https://wakatime.com/).

## Usage

Enable WakaTime for the current buffer by invoking `M-x wakatime-mode`.  If you wish to activate it globally, run `M-x global-wakatime-mode`.


## Configuration

Set variable `wakatime-api-key` to your [API key](https://wakatime.com/#apikey).
Point `wakatime-cli-path` to the absolute path of the CLI script (`wakatime-cli.py`).


## Installation

Heads Up! WakaTime depends on [Python](http://www.python.org/getit/) being installed to work correctly.

1. Install wakatime-mode for Emacs using [MELPA](http://melpa.milkbox.net/#/wakatime-mode).

2. Enter your [api key](https://wakatime.com/settings#apikey) from https://wakatime.com/settings#apikey, then press `enter`.

3. Download or clone the [wakatime-cli repository](https://github.com/wakatime/wakatime).

4. You will see a prompt asking for the path to wakatime-cli.py. Enter the path to the [wakatime-cli.py](https://github.com/wakatime/wakatime/blob/256aaf5dc3ffba35ea0b899b248328cccd76de6b/wakatime-cli.py) file, then press `enter`.

5. Add `(global-wakatime-mode)` to your `init.el` file, then restart Emacs.

6. Use Emacs with wakatime-mode turned on and your time will be tracked for you automatically.

7. Visit http://wakatime.com to see your logged time.


## Screen Shots

![Project Overview](https://wakatime.com/static/img/ScreenShots/ScreenShot-2014-10-29.png)
