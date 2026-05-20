# Emacs Minor Mode for WakaTime

`wakatime-mode` is an automatic time tracking extension for Emacs using [WakaTime](https://wakatime.com/).

## Installation

1. Install wakatime-mode for Emacs using [MELPA](https://melpa.org/#/wakatime-mode) (Doom users see [these instructions][doom install] instead).

2. Add `(global-wakatime-mode)` to your `init.el` file, then restart Emacs. wakatime-mode will automatically download the latest [wakatime-cli](https://github.com/wakatime/wakatime-cli/releases) into `~/.wakatime/` on first launch, and check for updates at most every 4 hours. To use a specific binary instead, set `wakatime-cli-path` to its absolute path (or install `wakatime-cli` into your `$PATH`, e.g. `brew install wakatime-cli` on macOS).

3. Enter your [api key](https://wakatime.com//api-key) in your `init.el` or `~/.wakatime.cfg` file ([config file format](https://github.com/wakatime/wakatime-cli/blob/develop/USAGE.md#ini-config-file)).

4. Use Emacs with wakatime-mode turned on and your time will be tracked for you automatically.

5. Visit http://wakatime.com to see your logged time.

### Installation for Spacemacs

See [Installing WakaTime with Spacemacs](https://develop.spacemacs.org/layers/+web-services/wakatime/README.html).

## Screen Shots

![Project Overview](https://wakatime.com/static/img/ScreenShots/ScreenShot-2014-10-29.png)

## Usage

Enable WakaTime for the current buffer by invoking `M-x wakatime-mode`. If you wish to activate it globally, run `M-x global-wakatime-mode`.

## Configuration

Set variable `wakatime-api-key` to your [API key](https://wakatime.com/api-key).

By default `wakatime-cli-path` is `nil`, which tells wakatime-mode to look for an existing `wakatime-cli` on your `$PATH` and otherwise auto-download the latest [wakatime-cli](https://github.com/wakatime/wakatime-cli/releases) release into `~/.wakatime/`. Set `wakatime-cli-path` to an absolute path to use a specific binary. Auto-update checks run at most every `wakatime-update-check-interval` seconds (4 hours by default).

## Troubleshooting

To be sure heartbeats are getting sent, turn on debug mode by adding this line to your `~/.wakatime.cfg` file:

    debug = true

Then run `tail -f ~/.wakatime/wakatime.log` and make sure you see a 201 response code from the [WakaTime API](https://wakatime.com/api).

[doom install]: https://medium.com/@el.gamerph/how-to-install-wakatime-in-doom-emacs-e5c582e15261
