# Emacs Minor Mode for WakaTime

`wakatime-mode` is an automatic time tracking extension for Emacs using [WakaTime](https://wakatime.com/).


## Installation

Heads Up! WakaTime depends on [Python](http://www.python.org/getit/) being installed to work correctly.

1. Install wakatime-mode for Emacs using [MELPA](https://melpa.org/#/wakatime-mode).
2. Install [wakatime-cli](https://pypi.python.org/pypi/wakatime) with `pip install wakatime`.
3. Add `(global-wakatime-mode)` to your `init.el` file, then restart Emacs.
4. You will see a prompt asking for the path to wakatime-cli. Run `which wakatime` and enter that path into the emacs prompt, then press `enter`.
5. Enter your [api key](https://wakatime.com/settings#apikey) in your `init.el` or `~/.wakatime.cfg` file.
6. Use Emacs with wakatime-mode turned on and your time will be tracked for you automatically.
7. Visit http://wakatime.com to see your logged time.

### Installation for Spacemacs

1. Add `wakatime-mode` to `dotspacemacs-additional-packages` in your `.spacemacs` file or manually install by `M-x package-install`.
2. Install [wakatime-cli](https://pypi.python.org/pypi/wakatime) with `pip install wakatime`.
3. Open the help buffer of wakatime-mode by `M-x describe-package`(`C-h P`) or selecting in Package Menu. Click `global-wakatime-mode`. Click `customize` and toggle to turn on it (non-nil). Alternatively, add `'(global-wakatime-mode t)` to `custom-set-variables` in your `.spacemacs` file.
4. In the help buffer, click `wakatime-api-key`. Click `customize` to set and save your API key for future sessions. Alternatively, add `'(wakatime-api-key "your-api-key")` to `custom-set-variables` in your `.spacemacs` file.
5. In the help buffer, click `wakatime-cli-path`. Click `customize` to set and save your CLI path for future sessions. Alternatively, add `'(wakatime-cli-path "your-cli-path")` to `custom-set-variables` in your `.spacemacs` file.
6. Make sure wakatime-mode is enabled. Visit http://wakatime.com to see your logged time.

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
