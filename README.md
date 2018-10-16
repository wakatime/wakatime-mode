# Emacs Minor Mode for ActivityWatch

`activity-watch-mode` is an automatic time tracking extension for Emacs using [ActivityWatch](https://activitywatch.net/).

## Installation

Heads Up! ActivityWatch depends on [request.el](https://tkf.github.io/emacs-request/) and [Projectile](https://github.com/bbatsov/projectile) being installed to work correctly.

1. Install activity-watch-mode for Emacs using [MELPA](https://melpa.org/#/activity-watch-mode).

3. Add `(global-activity-watch-mode)` to your `init.el` file, then restart Emacs.

6. Use Emacs with activity-watch-mode turned on and your time will be tracked for you automatically.

7. Visit http://localhost:5600 to see your logged time.

## Usage

Enable ActivityWatch for the current buffer by invoking `M-x activity-watch-mode`.  If you wish to activate it globally, run `M-x global-activity-watch-mode`.


## Configuration

Set variable `activity-watch-api-host` to your activity watch local instance (default to `http://localhost:5600`).

## Acknowledgments

This mode is based of the [wakatime-mode](https://github.com/wakatime/wakatime-mode).
