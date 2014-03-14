# Emacs Minor Mode for WakaTime

`wakatime-mode` is an automatic time tracking extension for Emacs using [WakaTime](https://wakatime.com/).

## Usage

Enable WakaTime for the current buffer by invoking `M-x wakatime-mode`.  If you wish to activate it globally, run `M-x global-wakatime-mode`.


## Configuration

Set variable `wakatime-api-key` to your [API key](https://wakatime.com/#apikey).
Point `wakatime-cli-path` to the absolute path of the CLI script (`wakatime-cli.py`).


## Installation

Heads Up! WakaTime depends on [Python](http://www.python.org/getit/) being installed to work correctly.

1. Get an API key from: http://wakatime.com.

2. Install wakatime-mode for Emacs using [MELPA](http://melpa.milkbox.net/#/wakatime-mode).

3. You will see a prompt asking for your [API key](https://wakatime.com/#apikey). Enter your API key, then press `enter`.

4. Download or clone the [wakatime-cli repository](https://github.com/wakatime/wakatime).

5. You will see a prompt asking for the path to wakatime-cli.py. Enter the path to the [wakatime-cli.py](https://github.com/wakatime/wakatime/blob/256aaf5dc3ffba35ea0b899b248328cccd76de6b/wakatime-cli.py) file, then press `enter`.

6. Use Emacs with wakatime-mode turned on and your time will automatically be tracked for you.

7. Visit http://wakatime.com to see your logged time.

8. Consider installing [BIND9](https://help.ubuntu.com/community/BIND9ServerHowto#Caching_Server_configuration) to cache your repeated DNS requests: `sudo apt-get install bind9`


## Screen Shots

![Project Overview](https://wakatime.com/static/img/ScreenShots/Screen Shot 2013-10-26 at 5.04.01 PM.png)
